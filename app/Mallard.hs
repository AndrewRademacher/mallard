{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Config
import           Control.Lens               hiding (argument, noneOf)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Lens             hiding (text)
import           Database.Mallard
import qualified Hasql.Pool                 as Pool
import           Options.Applicative
import           Path
import           Path.IO

-- State

data AppState
    = AppState
        { _statePostgreConnection     :: Pool.Pool
        }

$(makeClassy ''AppState)

instance HasPostgreConnection AppState where postgreConnection = statePostgreConnection

-- Application

main :: IO ()
main = do
    modes <- execParser configParser
    case modes of
        CmdMigrate o          -> mainMigrate o
        CmdConfirmChecksums o -> mainConfirmChecksums o
        CmdVersion o          -> mainVersion o

mainVersion :: OptsVersion -> IO ()
mainVersion _ = putStrLn "mallard -- 0.6.1.3"

--

mainMigrate :: OptsMigrate -> IO ()
mainMigrate appOpts = do
    pool <- Pool.acquire (1, 30, appOpts ^. postgreSettings)
    let initState = AppState pool

    _ <- (flip runReaderT appOpts . flip runStateT initState) runMigrate
            `catchAll` (\e -> putStrLn (displayException e) >> return ((), initState))

    Pool.release pool

parseRelOrAbsDir :: (MonadThrow m, MonadCatch m, MonadIO m) => FilePath -> m (Path Abs Dir)
parseRelOrAbsDir file = parseAbsDir file `catch` (\(_::PathException) -> makeAbsolute =<< parseRelDir file)

runMigrate :: (MonadIO m, MonadCatch m, MonadReader OptsMigrate m, MonadState AppState m, MonadThrow m) => m ()
runMigrate = do
    --
    ensureMigratonSchema
    --
    appOpts <- ask
    root <- parseRelOrAbsDir (appOpts ^. rootDirectory . unpacked)
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
    when (appOpts ^. runTestsFlag) $
        runTests (Map.elems mTests)

--

mainConfirmChecksums :: OptsConfirmChecksums -> IO ()
mainConfirmChecksums appOpts = do
    pool <- Pool.acquire (1, 30, appOpts ^. postgreSettings)
    let initState = AppState pool

    _ <- (flip runReaderT appOpts . flip runStateT initState) runConfirmChecksums
        `catchAll` (\e -> putStrLn (displayException e) >> return ((), initState))

    Pool.release pool

runConfirmChecksums :: (MonadIO m, MonadCatch m, MonadReader OptsConfirmChecksums m, MonadState AppState m, MonadThrow m) => m ()
runConfirmChecksums = do
    --
    appOpts <- ask
    root <- parseRelOrAbsDir (appOpts ^. rootDirectory . unpacked)
    --
    (mPlanned, _) <- importDirectory root
    --
    mApplied <- getAppliedMigrations
    --

    cs <- checkAppliedMigrations mPlanned mApplied
    mapM_ showComparison cs

showComparison :: (MonadIO m) => DigestComparison -> m ()
showComparison (DigestComparison n p _ a match) = liftIO $ do
    putStrLn $ "" <> show n <> " : " <> (if match then "Valid" else "Invalid")
    putStrLn $ ""
    putStrLn $ "    " <> show p
    putStrLn $ "    " <> show a
    putStrLn $ ""
