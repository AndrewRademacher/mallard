{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Database.Mallard.Types where

import           Control.Lens
import           Crypto.Hash
import           Data.ByteString     (ByteString)
import           Data.FileEmbed
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import           Data.Int
import           Data.Text           (Text)
import           Data.Text.Lens
import           Path

class HasMigrationTable a where
    migrationTable :: Lens' a (HashMap MigrationId Migration)

newtype MigrationId = MigrationId { unMigrationId :: Text }
    deriving (Eq, Ord, Hashable)

instance Show MigrationId where
    show (MigrationId txt) = txt ^. unpacked

type MigrationDigest = Digest SHA256

data Migration
    = Migration
        { _migrationName        :: MigrationId
        , _migrationFile        :: Path Abs File
        , _migrationDescription :: Text
        , _migrationRequires    :: [MigrationId]
        , _migrationChecksum    :: MigrationDigest
        , _migrationScript      :: Text
        }
    deriving (Show)

$(makeClassy ''Migration)

scriptsMigrationSchema :: [(Int64, ByteString)]
scriptsMigrationSchema =
    [ (0, $(embedFile "sql/mallard/0000-setup.sql"))
    , (1, $(embedFile "sql/mallard/0001-applied-migrations.sql"))
    ]
