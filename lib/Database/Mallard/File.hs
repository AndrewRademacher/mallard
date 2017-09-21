{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Mallard.File
    ( scanDirectoryForFiles
    , importMigrations
    ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Crypto.Hash
import qualified Data.ByteString         as BS
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Database.Mallard.Parser
import           Database.Mallard.Types
import           Path
import           Path.IO
import           Text.Megaparsec

scanDirectoryForFiles :: (MonadIO m, MonadThrow m) => Path Abs Dir -> m [Path Abs File]
scanDirectoryForFiles dir = concat <$> walkDirAccum Nothing (\_ _ c -> return [c]) dir

importMigrations :: (MonadIO m, MonadThrow m) => Path Abs Dir -> [Path Abs File] -> m (HashMap MigrationId Migration)
importMigrations root files = do
    migrationNames <- mapM (\file -> return . MigrationId . T.pack . toFilePath =<< setFileExtension "" =<< stripDir root file) files
    migrations <- zipWithM importMigration migrationNames files
    return $ Map.fromList (fmap (\m -> (m ^. migrationName, m)) migrations)

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
