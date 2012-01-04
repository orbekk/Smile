module ParserTest where
import Test.HUnit

import Parser
import SmileTypes

parserTest :: String -> AeExpr -> Test
parserTest input expected =
    TestCase(assertEqual "number test," expected result)
    where result = parseOrDie input

numberTest :: Test
numberTest = parserTest "1" (Number 1)

addTest :: Test
addTest = parserTest "+ 1 2" (Add (Number 1) (Number 2))

exprTest :: Test
exprTest = parserTest "+ + 1 2 + 3 4" expected
    where
    expected = Add (Add (Number 1) (Number 2))
                   (Add (Number 3) (Number 4))

tests :: Test
tests = TestList [numberTest, addTest, exprTest]

main :: IO ()
main = do
    runTestTT tests
    return ()
