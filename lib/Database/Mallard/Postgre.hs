{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Database.Mallard.Postgre
    ( HasPostgreConnection (..)
    , DigestMismatchException (..)
    , ensureMigratonSchema
    , getAppliedMigrations
    , applyMigration
    , applyMigrations
    ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Crypto.Hash
import           Data.Byteable
import           Data.ByteString             (ByteString)
import           Data.Foldable
import qualified Data.HashMap.Strict         as Map
import           Data.Int
import           Data.Monoid
import           Data.String.Interpolation
import qualified Data.Text.Encoding          as T
import           Database.Mallard.Types
import           Database.Mallard.Validation
import qualified Hasql.Decoders              as D
import qualified Hasql.Encoders              as E
import qualified Hasql.Pool                  as Pool
import           Hasql.Query
import           Hasql.Session
import           Hasql.Transaction           (IsolationLevel (..), Mode (..),
                                              Transaction)
import qualified Hasql.Transaction           as HT
import qualified Hasql.Transaction.Sessions  as HT

class HasPostgreConnection a where
    postgreConnection :: Lens' a Pool.Pool

data MigrationSchemaVersion
    = NotInit
    | MigrationVersion Int64
    deriving (Eq, Show)

instance Ord MigrationSchemaVersion where
    compare NotInit NotInit                           = EQ
    compare NotInit (MigrationVersion _)              = LT
    compare (MigrationVersion _) NotInit              = GT
    compare (MigrationVersion a) (MigrationVersion b) = compare a b

ensureMigratonSchema :: (MonadIO m, MonadState s m, HasPostgreConnection s) => m ()
ensureMigratonSchema = do
    mversion <- getMigrationSchemaVersion
    let toApply =
            case mversion of
                NotInit -> scriptsMigrationSchema
                MigrationVersion v -> drop (fromIntegral (v + 1)) scriptsMigrationSchema
    flip mapM_ toApply $ \a@(version,_) -> do
        runDB $ HT.transaction Serializable Write $ applyMigrationSchemaMigraiton a
        liftIO $ putStrLn $ "Migrator Version: " <> show version

runDB :: (MonadIO m, MonadState s m, HasPostgreConnection s) => Session a -> m a
runDB session = do
    pool <- fmap (^. postgreConnection) get
    res <- liftIO $ Pool.use pool session
    case res of
        Left err    -> error $ show err
        Right value -> return value

getAppliedMigrations
    :: (MonadIO m, MonadState s m, HasPostgreConnection s)
    => m MigrationTable
getAppliedMigrations = runDB $ do
    lst <- query () (statement stmt encoder decoder True)
    return $ Map.fromList $ fmap (\m -> (m ^. migrationName, m)) lst
    where
        stmt = "SELECT name, description, requires, checksum, script_text FROM mallard.applied_migrations;"
        encoder = E.unit
        decoder = D.rowsList $ Migration
            <$> D.value (MigrationId <$> D.text)
            <*> D.value D.text
            <*> D.value (D.array (D.arrayDimension replicateM (D.arrayValue (MigrationId <$> D.text))))
            <*> D.value valueDigest
            <*> D.value D.text

valueDigest :: (HashAlgorithm a) => D.Value (Digest a)
valueDigest = D.custom $ \_ bs ->
    case digestFromByteString bs of
        Nothing -> Left "ByteString was incorrect length for selected Digest type."
        Just v -> Right v

applyMigrations :: (MonadIO m, MonadState s m, HasPostgreConnection s) => [Migration] -> m ()
applyMigrations = mapM_ applyMigration

applyMigration :: (MonadIO m, MonadState s m, HasPostgreConnection s) => Migration -> m ()
applyMigration m = do
    runDB $ HT.transaction Serializable Write $ do
        HT.sql (T.encodeUtf8 (m ^. migrationScript))
        HT.query m (statement stmt encoder decoder True)
    liftIO $ putStrLn $ "Applied migration: " <> show (m ^. migrationName)
    where
        stmt = "INSERT INTO mallard.applied_migrations (name, description, requires, checksum, script_text) VALUES ($1, $2, $3, $4, $5)"
        encoder =
            contramap (unMigrationId . _migrationName) (E.value E.text) <>
            contramap _migrationDescription (E.value E.text) <>
            contramap (fmap unMigrationId . _migrationRequires) (E.value (E.array (E.arrayDimension foldl' (E.arrayValue E.text)))) <>
            contramap (toBytes . _migrationChecksum) (E.value E.bytea) <>
            contramap _migrationScript (E.value E.text)
        decoder = D.unit


applyMigrationSchemaMigraiton :: (Int64, ByteString) -> Transaction ()
applyMigrationSchemaMigraiton (version, script) = do
    HT.sql script
    HT.query version (statement stmt encoder decoder True)
    where
        stmt = "INSERT INTO mallard.migrator_version (version) VALUES ($1)"
        encoder = E.value E.int8
        decoder = D.unit

getMigrationSchemaVersion
    :: (MonadIO m, MonadState s m, HasPostgreConnection s)
    => m MigrationSchemaVersion
getMigrationSchemaVersion = runDB $ do
    isInit <- isMigrationVersionZero
    if isInit
        then do
            version <- query () (statement stmt E.unit (D.maybeRow (D.value D.int8)) True)
            case version of
                Nothing -> return $ MigrationVersion 0
                Just x  -> return $ MigrationVersion x
        else return NotInit
    where
        stmt = "SELECT coalesce(max(version), 0) as max_version FROM mallard.migrator_version"

isMigrationVersionZero :: Session Bool
isMigrationVersionZero = do
    mtable <- query () (statement stmt E.unit (D.maybeRow (D.value D.text)) True)
    case mtable of
        Nothing -> return False
        Just _  -> return True
    where
        stmt = "SELECT table_name FROM information_schema.tables WHERE table_schema = 'mallard' AND table_name = 'migrator_version';"

-- Exceptions

data DigestSizeMismatchException
    = DigestSizeMismatchException
    deriving (Show)

instance Exception DigestSizeMismatchException where
    displayException _ = [str|
        The size of the applied migration's checksum is not valid. This may imply the
        algorithm used by this tool has changed.
    |]

    -- TODO: Add ability to reset all checksums in migration table.
