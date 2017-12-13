import Parse
import Commands
import Data.List
import Data.List.Split
import Expression
import Data.Maybe
--import Control.Monad.ST
--import Data.IORef
--import Data.STRef


--myGlobalVar :: IORef Store
--myGlobalVar = unsafePerformIO (newIORef 17)

main_store::Store->IO Store
main_store store = do
 putStr " Rentrez une chaine : "
 input <- getLine
 let args = splitOn " " input
 let command = getCommandByName (args !! 0) commandes
 if isCommand (args !! 0) commandes then do
   store <- (run command) args store
   main_store store
 else do
  case (parseExpression input) of
    Nothing -> return ()
    Just x -> print (eval store x)
  main_store store  
--exp1:: Expression Float
exp1 = (BinaryOperation Soustraction (BinaryOperation Addition (Variable "x") (Literal 45)) (Literal 7))

main = do
  main_store []
--eval [("x",65)] (BinaryOperation Addition (Literal 1) (Variable "x"))
-- let a = parseExp xs	
-- case a of Right s -> print s	
