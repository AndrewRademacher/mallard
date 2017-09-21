{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Mallard.File
    ( importMigrationDirectory
    , importMigrationFile
    ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.ByteString         as BS
import qualified Data.HashMap.Strict     as Map
import           Database.Mallard.Parser
import           Database.Mallard.Types
import           Path
import           Path.IO
import           Text.Megaparsec

scanDirectoryForFiles :: (MonadIO m, MonadThrow m) => Path Abs Dir -> m [Path Abs File]
scanDirectoryForFiles dir = concat <$> walkDirAccum Nothing (\_ _ c -> return [c]) dir

importMigrationDirectory :: (MonadIO m, MonadThrow m) => Path Abs Dir -> m MigrationTable
importMigrationDirectory root = do
    files <- scanDirectoryForFiles root
    migrations <- mapM importMigrationFile' files
    return $ Map.fromList (fmap (\m -> (m ^. migrationName, m)) (concat migrations))

importMigrationFile :: (MonadIO m, MonadThrow m) => Path Abs File -> m MigrationTable
importMigrationFile file = Map.fromList . fmap (\m -> (m ^. migrationName, m)) <$> importMigrationFile' file

importMigrationFile' :: (MonadIO m, MonadThrow m) => Path Abs File -> m [Migration]
importMigrationFile' file = do
    fileContent <- liftIO $ BS.readFile (toFilePath file)
    let parseResult = runParser parseMigrations (toFilePath file) fileContent
    case parseResult of
        Left er -> throw $ ParserException file er
        Right m -> return m
