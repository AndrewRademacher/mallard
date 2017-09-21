{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Mallard.File where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Crypto.Hash
import qualified Data.ByteString         as BS
import qualified Data.HashMap.Strict     as Map
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Database.Mallard.Parser
import           Database.Mallard.Types
import           Path
import           Path.IO
import           Text.Megaparsec

class HasRootDirectory a where
    rootDirectory :: Lens' a (Path Abs Dir)

class HasMigrationFiles a where
    migrationFiles :: Lens' a [Path Abs File]

scanRootDirectoryForFiles
    :: ( MonadIO m, MonadState s m, MonadThrow m
        , HasRootDirectory s, HasMigrationFiles s )
    => m ()
scanRootDirectoryForFiles = do
    rootDir <- fmap (^. rootDirectory) get
    filesAbs <- concat <$> walkDirAccum Nothing (\_ _ c -> return [c]) rootDir
    modify (& migrationFiles .~ filesAbs)

importMigrations
    :: ( MonadIO m, MonadState s m, MonadThrow m
        , HasRootDirectory s, HasMigrationFiles s, HasMigrationTable s)
    => m ()
importMigrations = do
    root <- fmap (^. rootDirectory) get
    files <- fmap (^. migrationFiles) get
    migrationNames <- mapM (\file -> return . MigrationId . T.pack . toFilePath =<< setFileExtension "" =<< stripDir root file) files
    migrations <- zipWithM importMigration migrationNames files
    modify (& migrationTable .~ Map.fromList (fmap (\m -> (m ^. migrationName, m)) migrations))

importMigration :: (MonadIO m, MonadThrow m) => MigrationId -> Path Abs File -> m Migration
importMigration name file = do
    fileContent <- liftIO $ BS.readFile (toFilePath file)
    let parseResult = runParser parseMigration (toFilePath file) fileContent
    case parseResult of
        Left er -> throw $ ParserException file er
        Right (description, requires, content) ->
            return $ Migration
                { _migrationName = name
                , _migrationFile = file
                , _migrationDescription = description
                , _migrationRequires = requires
                , _migrationChecksum = hash (T.encodeUtf8 content)
                , _migrationScript = content
                }
