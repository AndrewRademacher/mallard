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
import           Options.Applicative        hiding (Parser)
import           Options.Applicative
import           Options.Applicative.Text
import           Path
import           Path.IO

data OptsMigrate
    = OptsMigrate
        { _optsRootDirectory   :: Text
        , _optsPostgreSettings :: Sql.Settings
        , _optsRunTests        :: Bool
        }
    deriving (Show)

$(makeClassy ''OptsMigrate)

data OptsVersion
    = OptsVersion Bool
    deriving (Show)

$(makeClassy ''OptsVersion)

data Modes
    = ModeMigrate OptsMigrate
    | ModeVersion OptsVersion
    deriving (Show)

modesParser :: Parser Modes
modesParser =
        (ModeMigrate <$> optsMigrateParser)
    <|> (ModeVersion <$> optsVersionParser)

optsMigrateParser :: Parser OptsMigrate
optsMigrateParser = OptsMigrate
    <$> argument text (metavar "ROOT")
    <*> connectionSettings Nothing
    <*> flag False True (long "test" <> short 't' <> help "Run tests after migration.")

optsVersionParser :: Parser OptsVersion
optsVersionParser = OptsVersion
    <$> flag False True (long "version" <> short 'v' <> help "Display application version.")

data AppState
    = AppState
        { _statePostgreConnection     :: Pool.Pool
        }

$(makeClassy ''AppState)

instance HasPostgreConnection AppState where postgreConnection = statePostgreConnection

main :: IO ()
main = do
    modes <- execParser opts
    case modes of
        ModeMigrate o -> mainMigrate o
        ModeVersion o -> mainVersion o
    where
        opts = info (modesParser <**> helper)
            ( fullDesc
            <> progDesc "Apply migrations to a database server."
            <> header "mallard - applies SQL database migrations." )

mainVersion :: OptsVersion -> IO ()
mainVersion _ = putStrLn "mallard -- 0.6.1.2"

mainMigrate :: OptsMigrate -> IO ()
mainMigrate appOpts = do
    pool <- Pool.acquire (1, 30, appOpts ^. optsPostgreSettings)
    let initState = AppState pool

    _ <- (flip runReaderT appOpts . flip runStateT initState) run
            `catchAll` (\e -> putStrLn (displayException e) >> return ((), initState))

    Pool.release pool

parseRelOrAbsDir :: (MonadThrow m, MonadCatch m, MonadIO m) => FilePath -> m (Path Abs Dir)
parseRelOrAbsDir file = parseAbsDir file `catch` (\(_::PathException) -> makeAbsolute =<< parseRelDir file)

run :: (MonadIO m, MonadCatch m, MonadReader OptsMigrate m, MonadState AppState m, MonadThrow m) => m ()
run = do
    --
    ensureMigratonSchema
    --
    appOpts <- ask
    root <- parseRelOrAbsDir (appOpts ^. optsRootDirectory . unpacked)
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
    when (appOpts ^. optsRunTests) $
        runTests (Map.elems mTests)
