module Ops.TestOps where

import qualified Test

runTests :: Maybe String -> IO ()
runTests _ = Test.runAllTests