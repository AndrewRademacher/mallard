{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Mallard.Commands.RepairChecksum
    ( repairChecksum
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict        as Map
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

-- | Takes a new checksum of a migration which has changed, places the new
-- checksum in the database's migraiton table.
repairChecksum
    :: (MonadIO m, MonadThrow m)
    => H.Pool -- ^ Connection to the database upon which migrations will be applied.
    -> Path b Dir -- ^ Directory which contains migration scripts.
    -> MigrationId -- ^ The migration which will be repaired
    -> m ()
repairChecksum pool dir mid = do
    absDir <- makeAbsolute dir
    let initState = AppState pool

    void $ flip runStateT initState $ do
        (mPlanned, _) <- importDirectory absDir
        mApplied <- getAppliedMigrations

        case (Map.lookup mid mPlanned, Map.lookup mid mApplied) of
            (Just planned, Just _) -> do
                setChecksum mid (_migrationChecksum planned)
            _ -> error "Could not find migration in either directory or applied in database."
