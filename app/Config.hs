{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Config
    ( OptsMigrate (..)
    , OptsVersion (..)
    , Command (..)

    -- Migrate Lenses
    , optsRootDirectory
    , optsPostgreSettings
    , optsRunTests

    -- Version Lenses
    , optsVersion

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

data OptsMigrate
    = OptsMigrate
        { _optsRootDirectory   :: Text
        , _optsPostgreSettings :: Sql.Settings
        , _optsRunTests        :: Bool
        }
    deriving (Show)

$(makeClassy ''OptsMigrate)

data OptsVersion
    = OptsVersion
    deriving (Show)

$(makeClassy ''OptsVersion)

data Command
    = CmdMigrate OptsMigrate
    | CmdVersion OptsVersion
    deriving (Show)

configParser :: ParserInfo Command
configParser = info (commandParser <**> helper)
    ( fullDesc
    <> header "mallard - Database management for pedantic people." )

commandParser :: Parser Command
commandParser = subparser
    ( command "migrate" infoMigrateParser
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

infoVersionParser :: ParserInfo Command
infoVersionParser = info (cmdVersionParser <**> helper)
    (progDesc "Display application version.")

cmdVersionParser :: Parser Command
cmdVersionParser = pure (CmdVersion OptsVersion)
