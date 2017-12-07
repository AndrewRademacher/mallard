{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Mallard.Commands.ConfirmChecksums
    ( confirmChecksums
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Data.Monoid
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

-- | Compairs the checksums of each migration in the provided directory to see
-- if they have changed since being applied to the database.
confirmChecksums
    :: (MonadIO m, MonadThrow m)
    => H.Pool -- ^ Connection to the database upon which migrations will be applied.
    -> Path b Dir -- ^ Directory which contains migration scripts.
    -> m ()
confirmChecksums pool dir = do
    absDir <- makeAbsolute dir
    let initState = AppState pool

    void $ flip runStateT initState $ do
        (mPlanned, _) <- importDirectory absDir
        mApplied <- getAppliedMigrations

        cs <- checkAppliedMigrations mPlanned mApplied
        mapM_ showComparison cs

showComparison :: (MonadIO m) => DigestComparison -> m ()
showComparison (DigestComparison n p _ a match) = liftIO $ do
    putStrLn $ "" <> show n <> " : " <> (if match then "Valid" else "Invalid")
    putStrLn $ ""
    putStrLn $ "    " <> show p
    putStrLn $ "    " <> show a
    putStrLn $ ""


-- mainConfirmChecksums :: OptsConfirmChecksums -> IO ()
-- mainConfirmChecksums appOpts = do
--     pool <- Pool.acquire (1, 30, appOpts ^. postgreSettings)
--     let initState = AppState pool

--     _ <- (flip runReaderT appOpts . flip runStateT initState) runConfirmChecksums
--         `catchAll` (\e -> putStrLn (displayException e) >> return ((), initState))

--     Pool.release pool

-- runConfirmChecksums :: (MonadIO m, MonadCatch m, MonadReader OptsConfirmChecksums m, MonadState AppState m, MonadThrow m) => m ()
-- runConfirmChecksums = do
--     --
--     appOpts <- ask
--     root <- parseRelOrAbsDir (appOpts ^. rootDirectory . unpacked)
--     --
--     (mPlanned, _) <- importDirectory root
--     --
--     mApplied <- getAppliedMigrations
--     --

--     cs <- checkAppliedMigrations mPlanned mApplied
--     mapM_ showComparison cs
