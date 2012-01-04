module Main where
import Test.HUnit

import qualified ParserTest
import qualified EvalTest

myRun :: String -> Test -> IO ()
myRun label test = runTestTT (TestLabel label test) >> return ()

main :: IO ()
main = do
    myRun "ParserTest" ParserTest.tests
    myRun "EvalTest" EvalTest.tests
