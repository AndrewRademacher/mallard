{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Lens               hiding (argument, noneOf)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import           Data.Text                  (Text)
import           Data.Text.Lens             hiding (text)
import           Database.Mallard
import qualified Hasql.Connection           as Sql
import           Hasql.Options.Applicative
import qualified Hasql.Pool                 as Pool
import           Options.Applicative        hiding (Parser, runParser)
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

data AppState
    = AppState
        { _statePostgreConnection     :: Pool.Pool
        , _stateRootDirectory         :: Path Abs Dir
        , _stateMigrationFiles        :: [Path Abs File]
        , _statePlannedMigrationTable :: MigrationTable
        , _stateAppliedMigrationTable :: MigrationTable
        , _stateMigrationGraph        :: MigrationGraph
        }

$(makeClassy ''AppState)

instance HasPostgreConnection AppState where postgreConnection = statePostgreConnection
instance HasRootDirectory AppState where rootDirectory = stateRootDirectory
instance HasMigrationFiles AppState where migrationFiles = stateMigrationFiles
instance HasPlannedMigrationTable AppState where plannedMigrationTable = statePlannedMigrationTable
instance HasAppliedMigrationTable AppState where appliedMigrationTable = stateAppliedMigrationTable
instance HasMigrationGraph AppState where migrationGraph = stateMigrationGraph

main :: IO ()
main = do
    appOpts <- execParser opts
    pool <- Pool.acquire (1, 30, appOpts ^. optionsPostgreSettings)
    let initState = AppState pool $(mkAbsDir "/doesnt/exist") [] Map.empty Map.empty emptyMigrationGraph

    _ <- (flip runReaderT appOpts . flip runStateT initState) run
            `catchAll` (\e -> putStrLn (displayException e) >> return ((), initState))

    Pool.release pool
    where
        opts = info (appOptionsParser <**> helper)
            ( fullDesc
            <> progDesc "Apply migrations to a database server."
            <> header "migrator - applies PSQL database migrations." )

run :: (MonadIO m, MonadReader AppOptions m, MonadState AppState m, MonadThrow m) => m ()
run = do
    --
    appOpts <- ask
    root <- makeAbsolute =<< parseRelDir (appOpts ^. optionsRootDirectory . unpacked)
    modify (\s -> s & rootDirectory .~ root)
    --
    files <- scanDirectoryForFiles root
    modify (\s -> s & migrationFiles .~ files)
    --
    mPlanned <- importMigrations root files
    modify (\s -> s & plannedMigrationTable .~ mPlanned)
    --
    mApplied <- getAppliedMigrations
    modify (\s -> s & appliedMigrationTable .~ mApplied)
    --
    let mGraph = mkMigrationGraph mPlanned
    modify (\s -> s & migrationGraph .~ mGraph)
    --
    ensureMigratonSchema
    --

    validateAppliedMigrations mPlanned mApplied

    --
    let unapplied = getUnappliedMigrations mGraph (Map.keys mApplied)
    applyMigrations mPlanned unapplied
    --

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
