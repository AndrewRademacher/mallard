{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Mallard.Validation
    ( validateAppliedMigrations
    , validateAppliedMigration
    , AppliedMigrationMissingException (..)
    , DigestMismatchException (..)
    ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict       as Map
import           Data.String.Interpolation
import           Database.Mallard.Types

-- | Validates applied migrations against planned migrations
validateAppliedMigrations :: (MonadIO m) => MigrationTable -> MigrationTable -> m ()
validateAppliedMigrations planned applied = mapM_ (validateAppliedMigration planned) (Map.elems applied)

validateAppliedMigration :: (MonadIO m) => MigrationTable -> Migration -> m ()
validateAppliedMigration plan aMig =
    case Map.lookup mid plan of
        Nothing -> throw $ AppliedMigrationMissingException mid
        Just pMig ->
            let pCheck = pMig ^. migrationChecksum
                aCheck = aMig ^. migrationChecksum
            in if aCheck == pCheck
                then return ()
                else throw $ DigestMismatchException mid pCheck mid aCheck
    where
        mid = aMig ^. migrationName

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
