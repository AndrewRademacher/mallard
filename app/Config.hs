{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Config
    ( OptsMigrate
    , OptsConfirmChecksums
    , OptsVersion
    , Command (..)

    -- Shared Lenses
    , rootDirectory
    , postgreSettings

    -- Migrate Lenses
    , runTestsFlag

    , configParser
    ) where

import           Control.Lens              hiding (argument)
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Hasql.Connection          as Sql
import           Hasql.Options.Applicative
import           Options.Applicative       hiding (Parser)
import           Options.Applicative
import           Options.Applicative.Text

class HasRootDirectory a where rootDirectory :: Lens' a Text
class HasPostgreSettings a where postgreSettings :: Lens' a Sql.Settings

--

data OptsMigrate
    = OptsMigrate
        { _omigrateRootDirectory   :: Text
        , _omigratePostgreSettings :: Sql.Settings
        , _omigrateRunTests        :: Bool
        }
    deriving (Show)

$(makeClassy ''OptsMigrate)

instance HasRootDirectory OptsMigrate where rootDirectory = omigrateRootDirectory
instance HasPostgreSettings OptsMigrate where postgreSettings = omigratePostgreSettings

runTestsFlag :: Lens' OptsMigrate Bool
runTestsFlag = omigrateRunTests

--

data OptsConfirmChecksums
    = OptsConfirmChecksums
        { _oconfirmRootDirectory   :: Text
        , _oconfirmPostgreSettings :: Sql.Settings
        }
    deriving (Show)

$(makeClassy ''OptsConfirmChecksums)

instance HasRootDirectory OptsConfirmChecksums where rootDirectory = oconfirmRootDirectory
instance HasPostgreSettings OptsConfirmChecksums where postgreSettings = oconfirmPostgreSettings

--

data OptsVersion
    = OptsVersion
    deriving (Show)

data Command
    = CmdMigrate OptsMigrate
    | CmdConfirmChecksums OptsConfirmChecksums
    | CmdVersion OptsVersion
    deriving (Show)

configParser :: ParserInfo Command
configParser = info (commandParser <**> helper)
    ( fullDesc
    <> header "mallard - Database management for pedantic people." )

commandParser :: Parser Command
commandParser = subparser
    ( command "migrate" infoMigrateParser
    <> command "confirm-checksums" infoConfirmChecksumsParser
    <> command "version" infoVersionParser
    )

infoMigrateParser :: ParserInfo Command
infoMigrateParser = info (cmdMigrateParser <**> helper)
    (progDesc "Perform database migrations supplied by scripts in a directory structure.")

cmdMigrateParser :: Parser Command
cmdMigrateParser = CmdMigrate <$> (OptsMigrate
    <$> argument text (metavar "ROOT")
    <*> connectionSettings Nothing
    <*> flag False True (long "test" <> short 't' <> help "Run tests after migration."))

--

infoConfirmChecksumsParser :: ParserInfo Command
infoConfirmChecksumsParser = info (cmdConfirmChecksumsParser <**> helper)
    (progDesc "Check database migrations for mismatched migration scripts.")

cmdConfirmChecksumsParser :: Parser Command
cmdConfirmChecksumsParser = CmdConfirmChecksums <$> (OptsConfirmChecksums
    <$> argument text (metavar "ROOT")
    <*> connectionSettings Nothing)

--

infoVersionParser :: ParserInfo Command
infoVersionParser = info (cmdVersionParser <**> helper)
    (progDesc "Display application version.")

cmdVersionParser :: Parser Command
cmdVersionParser = pure (CmdVersion OptsVersion)
