module ParseTest where
import Test.HUnit
import Parse
import Expression

--testParseExpressionForEmptyString :: Test
testParseExpressionForEmptyString = 
    TestCase $ assertEqual "Should return Nothing"
                           Nothing (parseExpression (""::[Char]))

--testParseExpressionForOneLiteral :: Test
testParseExpressionForOneLiteral =
    TestCase $ assertEqual "Should return Lateral 2" (Just (Literal 2))
               (parseExpression ("2"::String))

--testParseExpressionForOneVarible :: Test
testParseExpressionForOneVarible =
    TestCase $ assertEqual "Should return Variable x" (Just (Variable "x"))
               (parseExpression ("x"::String))

main :: IO Counts
main = runTestTT $ TestList [testParseExpressionForEmptyString, testParseExpressionForOneLiteral,testParseExpressionForOneVarible]