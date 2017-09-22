{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Lens               hiding (argument, noneOf)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
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
        , _optionsRunTests        :: Bool
        }
    deriving (Show)

$(makeClassy ''AppOptions)

appOptionsParser :: Parser AppOptions
appOptionsParser = AppOptions
    <$> argument text (metavar "ROOT")
    <*> connectionSettings Nothing
    <*> flag False True (long "test" <> short 't' <> help "Run tests after migration.")

data AppState
    = AppState
        { _statePostgreConnection     :: Pool.Pool
        }

$(makeClassy ''AppState)

instance HasPostgreConnection AppState where postgreConnection = statePostgreConnection

main :: IO ()
main = do
    appOpts <- execParser opts
    pool <- Pool.acquire (1, 30, appOpts ^. optionsPostgreSettings)
    let initState = AppState pool

    _ <- (flip runReaderT appOpts . flip runStateT initState) run
            `catchAll` (\e -> putStrLn (displayException e) >> return ((), initState))

    Pool.release pool
    where
        opts = info (appOptionsParser <**> helper)
            ( fullDesc
            <> progDesc "Apply migrations to a database server."
            <> header "mallard - applies SQL database migrations." )

parseRelOrAbsDir :: (MonadThrow m, MonadCatch m, MonadIO m) => FilePath -> m (Path Abs Dir)
parseRelOrAbsDir file = parseAbsDir file `catch` (\(_::PathParseException) -> makeAbsolute =<< parseRelDir file)

run :: (MonadIO m, MonadCatch m, MonadReader AppOptions m, MonadState AppState m, MonadThrow m) => m ()
run = do
    --
    ensureMigratonSchema
    --
    appOpts <- ask
    root <- parseRelOrAbsDir (appOpts ^. optionsRootDirectory . unpacked)
    --
    (mPlanned, mTests) <- importDirectory root
    --
    mApplied <- getAppliedMigrations
    --
    let mGraph = fromJust $ mkMigrationGraph mPlanned
    --

    validateAppliedMigrations mPlanned mApplied

    --
    let unapplied = getUnappliedMigrations mGraph (Map.keys mApplied)
    toApply <- inflateMigrationIds mPlanned unapplied
    applyMigrations toApply
    --
    when (appOpts ^. optionsRunTests) $
        runTests (Map.elems mTests)
