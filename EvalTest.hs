module EvalTest where
import Test.HUnit

import Parser
import Eval
import SmileTypes

evalTest :: String -> AeExpr -> Test
evalTest input expected =
    TestCase(assertEqual "number test," expected result)
    where result = eval (parseOrDie input)

numberTest :: Test
numberTest = evalTest "1" (Number 1)

addTest :: Test
addTest = evalTest "+ 1 2" (Number 3)

addTest2 :: Test
addTest2 = evalTest "+ 1 + 3 4" (Number 8)

tests :: Test
tests = TestList [numberTest, addTest, addTest2]

main :: IO ()
main = do
    runTestTT tests
    return ()
