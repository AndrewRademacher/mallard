import qualified Test.Integration
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All"
    [ Test.Integration.tests
    ]
