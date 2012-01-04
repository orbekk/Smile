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
addTest = evalTest "(+ 1 2)" (Number 3)

addTest2 :: Test
addTest2 = evalTest "(+ 1 (+ 3 4))" (Number 8)

withTest :: Test
withTest = evalTest "(with (x 10) x)" (Number 10)

withTest2 :: Test
withTest2 = evalTest "(with (x 10) (with (y 11) y))" (Number 11)

withScope :: Test
withScope = evalTest "(with (x 10) (with (x 12) x))" (Number 12)

withBinding :: Test
withBinding = evalTest "(with (x 1) \
\                             (with (x (+ x 1)) \
\                                   x))" (Number 2)

withBinding2 :: Test
withBinding2 = evalTest "(with (x 1) (with (y x) y))" (Number 1)

funExpr :: Test
funExpr = evalTest "((fun (x) (+ x 1)) 1)" (Number 2)

funSubstTest :: Test
funSubstTest = evalTest "(with (x 10) ((fun (y) x) 1))" (Number 10)

tests :: Test
tests = TestList [numberTest, addTest, addTest2, withTest, withTest2,
                  withScope, withBinding, withBinding2, funExpr,
                  funSubstTest]

main :: IO ()
main = do
    runTestTT tests
    return ()
