{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Mallard.Parser
    ( ParserException (..)
    , Action (..)
    , parseActions
    , parseMigration
    , parseTest
    ) where

import           Control.Exception          hiding (try)
import           Control.Monad
import           Crypto.Hash
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.String.Interpolation
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Void
import           Database.Mallard.Types
import           Path
import           Text.Megaparsec            hiding (parseTest)
import           Text.Megaparsec.Char       hiding (tab)
import qualified Text.Megaparsec.Char.Lexer as L

data Action
    = ActionMigration Migration
    | ActionTest Test

type Description = Text
type Requires = MigrationId

data FieldValue
    = TextField Text
    | ListField [Text]

type Parser = Parsec Void Text

-- Lexer

spaceConsumer :: Parser ()
spaceConsumer = L.space space' (L.skipLineComment "@") (L.skipBlockComment "@@" "@@")
    where
        space' = void (try spaceChar <|> char '-')

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

symbol' :: Text -> Parser Text
symbol' = L.symbol' spaceConsumer

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

semiColon :: Parser Text
semiColon = symbol ";"

migrationS :: Parser Text
migrationS = symbol' "migration"

testS :: Parser Text
testS = symbol' "test"

sbang :: Parser Text
sbang = symbol "#!"

sbangOrEof :: Parser ()
sbangOrEof = try (void sbang) <|> eof

atom :: Parser String
atom = do
    val <- many alphaNumChar
    spaceConsumer
    return val

-- Parser

parseActions :: Parser [Action]
parseActions = spaceConsumer >> sbang >> manyTill parseAction eof
    where
        parseAction =
            try (ActionMigration <$> parseMigration)
            <|> (ActionTest <$> parseTest)


parseTest :: Parser Test
parseTest = do
    (name, description) <- parseTestHeader
    content <- T.pack <$> manyTill anyChar sbangOrEof
    return $ Test
        { _testName = name
        , _testDescription = description
        , _testScript = content
        }

parseTestHeader :: Parser (TestId, Description)
parseTestHeader = do
    testS
    fields <- parseHeaderFields
    semiColon
    case Map.lookup "name" fields of
        Nothing -> fail "The name field was not provided in the header."
        Just (ListField _) -> fail "The name field cannot be a list."
        Just (TextField name) ->
            case Map.lookup "description" fields of
                Nothing -> fail "The description field was not provided in the header."
                Just (ListField _) -> fail "The description field cannot be a list."
                Just (TextField description) -> return (TestId name, description)

parseMigration :: Parser Migration
parseMigration = do
    (name, description, requires) <- parseMigrationHeader
    content <- T.pack <$> manyTill anyChar sbangOrEof
    return $ Migration
        { _migrationName = name
        , _migrationDescription = description
        , _migrationRequires = requires
        , _migrationChecksum = hash (T.encodeUtf8 content)
        , _migrationScript = content
        }

parseMigrationHeader :: Parser (MigrationId, Description, [Requires])
parseMigrationHeader = do
    migrationS
    fields <- parseHeaderFields
    semiColon
    case Map.lookup "name" fields of
        Nothing -> fail "The name field was not provided in the header."
        Just (ListField _) -> fail "The name field cannot be a list."
        Just (TextField name) ->
            case Map.lookup "description" fields of
                Nothing -> fail "The description field was not provided in the header."
                Just (ListField _) -> fail "The description field cannot be a list."
                Just (TextField description) ->
                    case Map.lookup "requires" fields of
                        Nothing                   -> return (MigrationId name, description, [])
                        Just (TextField requires) -> return (MigrationId name, description, [MigrationId requires])
                        Just (ListField requires) -> return (MigrationId name, description, fmap MigrationId requires)

parseFieldValue :: Parser FieldValue
parseFieldValue = parseTextValue <|> parseListValue
    where
        parseTextValue = TextField <$> parseQuotedText
        parseListValue = ListField <$> brackets (parseQuotedText `sepBy` comma)

parseHeaderFields :: Parser (HashMap Text FieldValue)
parseHeaderFields = Map.fromList <$> field `sepBy` comma
    where
        field :: Parser (Text, FieldValue)
        field = do
            name <- atom
            colon
            value <- parseFieldValue
            return (T.pack name, value)

parseQuotedText :: Parser Text
parseQuotedText = T.pack <$> between (symbol "\"") (symbol "\"") (many (noneOf ("\""::String)))


-- Exceptions

data ParserException
    = ParserException
        { _peFile  :: (Path Abs File)
        , _peError :: ParseError Char Void
        }
    deriving (Show)

instance Exception ParserException where
    displayException e = [str|
        Unable to parse file: $:toFilePath (_peFile e)$

        $tab$Line: $:sourceLine (NonEmpty.head (errorPos (_peError e)))$
        $tab$Column: $:sourceColumn (NonEmpty.head (errorPos (_peError e)))$
        $tab$Item: $:_peError e$
    |]
