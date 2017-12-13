module ExpressionTest where

import Test.HUnit
import Parse
import Expression


testEmptyStore :: Test
testEmptyStore =
 TestCase $ assertEqual "Should return" (Nothing) (eval [] exp)
               where
                exp = (BinaryOperation Soustraction (BinaryOperation Addition (Variable "x") (Literal 45)) (Literal 7))


testStore :: Test
testStore =
 TestCase $ assertEqual "Should return" (Just 40.0) (eval [("x",2)] exp)
               where
                exp = (BinaryOperation Soustraction (BinaryOperation Addition (Variable "x") (Literal 45)) (Literal 7))

testUnaryOperation :: Test
testUnaryOperation =
 TestCase $ assertEqual "Should return" (Just (-45.0)) (eval [] exp)
               where
                exp = (UnaryOperation Neg (Literal 45))  

testBinaryOperation :: Test
testBinaryOperation =
 TestCase $ assertEqual "Should return" (Just (41.0)) (eval [("x",3)] exp)
               where
               exp = (BinaryOperation Soustraction (BinaryOperation Addition (Variable "x") (Literal 45)) (Literal 7))

main :: IO Counts
main = runTestTT $ TestList [testEmptyStore, testStore, testUnaryOperation, testBinaryOperation]
