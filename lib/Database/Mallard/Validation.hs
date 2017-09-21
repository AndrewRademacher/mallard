{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Mallard.Validation
    ( validateChecksum
    , AppliedMigrationMissingException (..)
    , DigestMismatchException (..)
    ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict        as Map
import           Data.Int
import           Data.String.Interpolation
import           Database.Mallard.Types

validateChecksum
    :: ( MonadState s m
        , HasMigrationTable s )
    => (Int64, MigrationId, MigrationDigest) -> m ()
validateChecksum (_, mid, digest) = do
    mTable <- fmap (^. migrationTable) get
    case Map.lookup mid mTable of
        Nothing -> throw $ AppliedMigrationMissingException mid
        Just mig ->
            if (mig ^. migrationChecksum) == digest
                then return ()
                else throw $ DigestMismatchException mid (mig ^. migrationChecksum) mid digest

-- Exceptions

data AppliedMigrationMissingException
    = AppliedMigrationMissingException
        { _ammeAppliedMigrationName :: MigrationId
        }
    deriving (Show)

instance Exception AppliedMigrationMissingException where
    displayException e = [str|
        A migration that was previously applied is missing from the current migration plan.

        $tab$Applied Migration: $:_ammeAppliedMigrationName e$
    |]

data DigestMismatchException
    = DigestMismatchException
        { _dmePlannedMigrationName     :: MigrationId
        , _dmePlannedMigrationChecksum :: MigrationDigest
        , _dmeAppliedMigrationName     :: MigrationId
        , _dmeAppliedMigrationChecksum :: MigrationDigest
        }
    deriving (Show)

instance Exception DigestMismatchException where
    displayException d = [str|
        Mis-matching checksums indicate that a migration file has changed since it was applied.

        $tab$Planned Migration: $:_dmePlannedMigrationName d$ ($:_dmePlannedMigrationChecksum d$)
        $tab$Applied Migration: $:_dmeAppliedMigrationName d$ ($:_dmeAppliedMigrationChecksum d$)
    |]
