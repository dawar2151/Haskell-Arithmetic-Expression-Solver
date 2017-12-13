module ExpressionTest where

import Test.HUnit
import Parse
import Expression

exp1 = (BinaryOperation Division (BinaryOperation Addition (Variable "x") (Literal 45)) (Literal 7))
exp2 = (UnaryOperation Sin (Literal 45))

testSin :: Test
testSin =
 TestCase $ assertEqual "Should return" (Just 2)
               (eval [] exp2)

testDivision :: Test
testDivision =
 TestCase $ assertEqual "Should return" (Just 3)
               (eval [("x", 4)] exp1)              

main :: IO Counts
main = runTestTT $ TestList [testSin, testDivision]
