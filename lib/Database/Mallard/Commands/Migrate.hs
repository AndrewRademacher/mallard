{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Mallard.Commands.Migrate
    ( migrate
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
import           Database.Mallard
import qualified Hasql.Pool                 as H
import           Path
import           Path.IO

data AppState
    = AppState
        { _statePool :: H.Pool
        }

$(makeClassy ''AppState)

instance HasPostgreConnection AppState where postgreConnection = statePool

-- | Run all unapplied migrations in a directory.
migrate
    :: (MonadIO m, MonadThrow m, MonadCatch m)
    => H.Pool -- ^ Connection to the database upon which migrations will be applied.
    -> Path b Dir -- ^ Directory which contains migration scripts.
    -> Bool -- ^ If True, run test scripts after migration.
    -> m ()
migrate conn dir runTestsFlag = do
    absDir <- makeAbsolute dir
    let initState = AppState conn

    void $ flip runStateT initState $ do
        ensureMigratonSchema
        (mPlanned, mTests) <- importDirectory absDir
        mApplied <- getAppliedMigrations
        let mGraph = fromJust $ mkMigrationGraph mPlanned

        validateAppliedMigrations mPlanned mApplied

        let unapplied = getUnappliedMigrations mGraph (Map.keys mApplied)
        toApply <- inflateMigrationIds mPlanned unapplied
        applyMigrations toApply

        when runTestsFlag $
            runTests (Map.elems mTests)


-- mainMigrate :: OptsMigrate -> IO ()
-- mainMigrate appOpts = do
--     pool <- Pool.acquire (1, 30, appOpts ^. postgreSettings)
--     let initState = AppState pool

--     _ <- (flip runReaderT appOpts . flip runStateT initState) runMigrate
--             `catchAll` (\e -> putStrLn (displayException e) >> return ((), initState))

--     Pool.release pool

-- parseRelOrAbsDir :: (MonadThrow m, MonadCatch m, MonadIO m) => FilePath -> m (Path Abs Dir)
-- parseRelOrAbsDir file = parseAbsDir file `catch` (\(_::PathException) -> makeAbsolute =<< parseRelDir file)

-- -- runMigrate :: (MonadIO m, MonadCatch m, MonadReader OptsMigrate m, MonadState AppState m, MonadThrow m) => m ()
-- runMigrate = do
--     --
--     ensureMigratonSchema
--     --
--     appOpts <- ask
--     root <- parseRelOrAbsDir (appOpts ^. rootDirectory . unpacked)
--     --
--     (mPlanned, mTests) <- importDirectory root
--     --
--     mApplied <- getAppliedMigrations
--     --
--     let mGraph = fromJust $ mkMigrationGraph mPlanned
--     --

--     validateAppliedMigrations mPlanned mApplied

--     --
--     let unapplied = getUnappliedMigrations mGraph (Map.keys mApplied)
--     toApply <- inflateMigrationIds mPlanned unapplied
--     applyMigrations toApply
--     --
--     when (appOpts ^. runTestsFlag) $
--         runTests (Map.elems mTests)
