{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Lens                      hiding (argument, noneOf)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as Map
import           Data.Monoid
import           Data.Text                         (Text)
import           Data.Text.Lens                    hiding (text)
import           Database.Mallard
import qualified Hasql.Connection                  as Sql
import           Hasql.Options.Applicative
import qualified Hasql.Pool                        as Pool
import           Options.Applicative               hiding (Parser, runParser)
import           Options.Applicative
import           Options.Applicative.Text
import           Path
import           Path.IO

data AppOptions
    = AppOptions
        { _optionsRootDirectory   :: Text
        , _optionsPostgreSettings :: Sql.Settings
        }
    deriving (Show)

$(makeClassy ''AppOptions)

appOptionsParser :: Parser AppOptions
appOptionsParser = AppOptions
    <$> argument text (metavar "ROOT")
    <*> connectionSettings Nothing

data AppEnvironment
    = AppEnvironment
        { _environmentAppOptions    :: AppOptions
        , _environmentRootDirectory :: Path Abs Dir
        }

$(makeClassy ''AppEnvironment)

instance HasAppOptions AppEnvironment where appOptions = environmentAppOptions
instance HasRootDirectory AppEnvironment where rootDirectory = environmentRootDirectory

data AppState
    = AppState
        { _statePostgreConnection  :: Pool.Pool
        , _stateMigrationFiles     :: [Path Abs File]
        , _stateMigrationTable     :: HashMap MigrationId Migration
        , _stateMigrationNodeTable :: HashMap MigrationId G.Node
        , _stateMigrationGraph     :: G.Gr MigrationId ()
        }

$(makeClassy ''AppState)

instance HasPostgreConnection AppState where postgreConnection = statePostgreConnection
instance HasMigrationFiles AppState where migrationFiles = stateMigrationFiles
instance HasMigrationTable AppState where migrationTable = stateMigrationTable
instance HasMigrationNodeTable AppState where migrationNodeTable = stateMigrationNodeTable
instance HasMigrationGraph AppState where migrationGraph = stateMigrationGraph

main :: IO ()
main = do
    appOpts <- execParser opts
    root <- makeAbsolute =<< parseRelDir (appOpts ^. optionsRootDirectory . unpacked)
    let env = AppEnvironment appOpts root

    pool <- Pool.acquire (1, 30, appOpts ^. optionsPostgreSettings)
    let initState = AppState pool [] Map.empty Map.empty G.empty

    _ <- (flip runReaderT env . flip runStateT initState) run
            `catchAll` (\e -> putStrLn (displayException e) >> return ((), initState))

    Pool.release pool
    where
        opts = info (appOptionsParser <**> helper)
            ( fullDesc
            <> progDesc "Apply migrations to a database server."
            <> header "migrator - applies PSQL database migrations." )

run :: (MonadIO m, MonadReader AppEnvironment m, MonadState AppState m, MonadThrow m) => m ()
run = do
    scanRootDirectoryForFiles
    importMigrations

    generateMigrationGraph

    ensureMigratonSchema
    ensureApplicationSchema

    -- New application plan.

    -- Add root folder to state (store in state)
    -- Scan root directory for files (store in state)
    -- Import migrations from files found (store in state) (planed migrations)
    -- Import migrations from database (store in state) (applied migrations)
    -- Generate migration graph (store in state) (merge nodeTable and graph into unified opaque type)
    -- Find unapplied migrations (store in state)

    -- Validate application state
        -- Validate that all applied migrations exist in planned migrations.
        -- Validate that all applied migrations have matching checksums with their planned migration counterparts.
        -- Validate that there are no circular references in the migration graph.

    -- Apply unapplied migrations.
