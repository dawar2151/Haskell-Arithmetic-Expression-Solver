module ExpressionTest where

import Test.HUnit
import Parse
import Commands

isCommandTest :: Test
isCommandTest =
 TestCase $ assertEqual "Should return" (True)
               (isCommand "quit" commandes)
                            

main :: IO Counts
main = runTestTT $ TestList [isCommandTest]
