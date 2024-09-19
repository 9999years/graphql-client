import Test.Tasty (defaultMain, testGroup)

import Data.GraphQL.Test.Error
import Data.GraphQL.Test.Generation.TestGeneration
import Data.GraphQL.Test.Monad.Class
import Data.GraphQL.Test.TestUtils

main :: IO ()
main =
  defaultMain $
    testGroup
      "graphql-client"
      [ testRunQuery
      , testTestUtils
      , testGeneration
      , testErrorDisplay
      ]
