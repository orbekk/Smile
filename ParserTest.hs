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
addTest = parserTest "(+ 1 2)" (Add (Number 1) (Number 2))

idTest :: Test
idTest = parserTest "x" (Id "x")

exprTest :: Test
exprTest = parserTest "(+ (+ 1 2) (+ 3 4))" expected
    where
    expected = Add (Add (Number 1) (Number 2))
                   (Add (Number 3) (Number 4))

withTest :: Test
withTest = parserTest "(with (x 10) (+ x x))" expected
    where
    expected = With (Id "x") (Number 10) (Add (Id "x") (Id "x"))

exprTest2 :: Test
exprTest2 = parserTest "(+ x (+ 3 4))" expected
    where
    expected = Add (Id "x") (Add (Number 3) (Number 4))

functionTest :: Test
functionTest = parserTest "(fun (x) 1)" expected
    where
    expected = (Function (Id "x") (Number 1))

appTest :: Test
appTest = parserTest "(x y)" (App (Id "x") (Id "y"))

tests :: Test
tests = TestList [numberTest, addTest, exprTest, exprTest2, idTest, withTest,
                  functionTest, appTest]

main :: IO ()
main = do
    runTestTT tests
    return ()
