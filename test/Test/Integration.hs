{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Integration (tests) where

import           Data.Functor.Contravariant
import           Data.Monoid
import           Database.Mallard.Commands.Migrate
import qualified Hasql.Decoders                    as HD
import qualified Hasql.Encoders                    as HE
import qualified Hasql.Pool                        as H
import qualified Hasql.Query                       as H
import qualified Hasql.Session                     as H
import           Path
import           Test.Tasty
import           Test.Tasty.HUnit
import           Turtle

tests :: TestTree
tests = testGroup "Integration"
    [ withResource openContactDB closeDB testContactDeploy
    ]

openContactDB :: IO H.Pool
openContactDB = do
    resCreateDb <- proc "createdb" ["contacts"] empty
    case resCreateDb of
        ExitSuccess -> H.acquire (1, 30, "host=localhost user=vagrant dbname=contacts password='password'")
        -- ExitSuccess -> do
            -- mConn <- H.acquire "host=localhost user=vagrant dbname=contacts password='password'"
            -- case mConn of
            --     Left er -> error $ show er
            --     Right v -> return v
        ExitFailure ec -> error $ "createdb command failed with code " <> show ec

closeDB :: H.Pool -> IO ()
closeDB conn = do
    H.release conn
    dropRes <- proc "dropdb" ["contacts"] empty
    case dropRes of
        ExitSuccess -> return ()
        ExitFailure ec -> error $ "dropdb command failed with code " <> show ec

testContactDeploy :: IO H.Pool -> TestTree
testContactDeploy getConnection = testCaseSteps "Contact Deploy" $ \step -> do
    conn <- getConnection

    step "Migrate contacts database."
    migrate conn $(mkRelDir "sql/example-contacts") True

    step "Check insert of person."
    iRes <- H.use conn (H.query ("John", "Edward", "Doe") insertPerson)
    case iRes of
        Left e'  -> error $ show e'
        Right i -> do

            step "Check recovery of person."
            sRes <- H.use conn (H.query i getPerson)
            case sRes of
                Left e'' -> error $ show e''
                Right (a, b, c) -> do
                    assertEqual "" "John" a
                    assertEqual "" "Edward" b
                    assertEqual "" "Doe" c

insertPerson :: H.Query (Text, Text, Text) Int
insertPerson = H.statement stmt encoder decoder True
    where
        stmt = "INSERT INTO person(name_first, name_middle, name_last) VALUES ($1, $2, $3) RETURNING id;"
        encoder =
            contramap (\(a, _, _) -> a) (HE.value HE.text) <>
            contramap (\(_, b, _) -> b) (HE.value HE.text) <>
            contramap (\(_, _, c) -> c) (HE.value HE.text)
        decoder = HD.singleRow $ fromIntegral
            <$> (HD.value HD.int8)

getPerson :: H.Query Int (Text, Text, Text)
getPerson = H.statement stmt encoder decoder True
    where
        stmt = "SELECT name_first, name_middle, name_last FROM person WHERE id = $1;"
        encoder =
            contramap fromIntegral (HE.value HE.int8)
        decoder = HD.singleRow $ (,,)
            <$> (HD.value HD.text)
            <*> (HD.value HD.text)
            <*> (HD.value HD.text)
