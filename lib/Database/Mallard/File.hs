{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Mallard.File
    ( importDirectory
    , importFile
    ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.ByteString         as BS
import           Data.Foldable
import qualified Data.HashMap.Strict     as Map
import           Database.Mallard.Parser
import           Database.Mallard.Types
import           Path
import           Path.IO
import           Text.Megaparsec

scanDirectoryForFiles :: (MonadIO m, MonadThrow m) => Path Abs Dir -> m [Path Abs File]
scanDirectoryForFiles dir = concat <$> walkDirAccum Nothing (\_ _ c -> return [c]) dir

importDirectory :: (MonadIO m, MonadThrow m) => Path Abs Dir -> m (MigrationTable, TestTable)
importDirectory root = do
    files <- scanDirectoryForFiles root
    migrations <- mapM importFile' files
    return $ sortActions $ concat migrations

importFile :: (MonadIO m, MonadThrow m) => Path Abs File -> m (MigrationTable, TestTable)
importFile = fmap sortActions . importFile'

sortActions :: [Action] -> (MigrationTable, TestTable)
sortActions actions = foldl' sortFn (Map.empty, Map.empty) actions
    where
        sortFn (mm, tm) (ActionMigration m) = (Map.insert (m ^. migrationName) m mm, tm)
        sortFn (mm, tm) (ActionTest t)      = (mm, Map.insert (t ^. testName) t tm)

importFile' :: (MonadIO m, MonadThrow m) => Path Abs File -> m [Action]
importFile' file = do
    fileContent <- liftIO $ BS.readFile (toFilePath file)
    let parseResult = runParser parseActions (toFilePath file) fileContent
    case parseResult of
        Left er -> throw $ ParserException file er
        Right m -> return m
