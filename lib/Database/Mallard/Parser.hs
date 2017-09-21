{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Mallard.Parser where

import           Control.Exception
import           Control.Lens               hiding (noneOf)
import           Control.Monad
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.String.Interpolation
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Lens
import           Database.Mallard.Types
import           Path
import           Text.Megaparsec            hiding (tab)
import           Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer      as L

type Description = Text
type Requires = MigrationId
type Script = Text

data FieldValue
    = TextField Text
    | ListField [Text]

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "##" "##")

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

comma :: Parser ()
comma = char ','  >> space

parseMigration :: Parser (MigrationId, Description, [Requires], Script)
parseMigration = do
    (name, description, requires) <- parseHeader
    content <- T.pack <$> manyTill anyChar eof
    return (name, description, requires, content)

parseHeader :: Parser (MigrationId, Description, [Requires])
parseHeader = do
    fields <- parseHeaderFields
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
parseHeaderFields = Map.fromList <$> between (symbol "--|") (symbol "--|") (field `sepBy` newline)
    where
        field :: Parser (Text, FieldValue)
        field = do
            string "-- "
            name <- many (noneOf (" :"::String))
            space
            char ':'
            space
            value <- parseFieldValue
            return (T.pack name, value)

parseQuotedText :: Parser Text
parseQuotedText = do
    char '"'
    val <- many (noneOf ("\""::String))
    char '"'
    return $ val ^. packed

-- Exceptions

data ParserException
    = ParserException
        { _peFile  :: (Path Abs File)
        , _peError :: ParseError Char Dec
        }
    deriving (Show)

instance Exception ParserException where
    displayException e = [str|
        Unable to parse file: $:toFilePath (_peFile e)$

        $tab$Line: $:sourceLine (NonEmpty.head (errorPos (_peError e)))$
        $tab$Column: $:sourceColumn (NonEmpty.head (errorPos (_peError e)))$
        $tab$Expected: $:errorExpected (_peError e)$
        $tab$Occurred: $:errorUnexpected (_peError e)$
    |]
