module Expression(
 Store(..),
 Variable,
 Expression(..),
 BinaryOperator(..),
 UnaryOperator(..),
 eval,
)
where
import Data.List
import Data.Maybe

-- store type associate v to key
type Store = [(String, Double)]

type Variable = [Char]

data BinaryOperator = 
     Addition
     |Multiplication
     |Soustraction
     |Division
     |Exponentiation
     deriving (Show, Eq)

-- Type Unaiare
data UnaryOperator = 
 Neg
 deriving (Show, Eq)  
-- Type recursif Expression
data Expression=
 Literal Double
 | Variable String
 | UnaryOperation UnaryOperator Expression
 | BinaryOperation BinaryOperator Expression Expression
 deriving (Show, Eq)




eval :: Store -> Expression -> Maybe Double
eval table (Literal n) = Just n
eval table (Variable x) = lookup x table
eval table (UnaryOperation Neg exp)  = maybeEvalUnaire (negate) (eval table exp)
eval table (BinaryOperation Multiplication exp1 exp2)  = maybeEvalBinaire (*) (eval table exp1) (eval table exp2)
eval table (BinaryOperation Addition exp1 exp2)  = maybeEvalBinaire (+) (eval table exp1) (eval table exp2)
eval table (BinaryOperation Soustraction exp1 exp2)  = maybeEvalBinaire (-) (eval table exp1) (eval table exp2)
eval table (BinaryOperation Exponentiation exp1 exp2)  = maybeEvalBinaire (**) (eval table exp1) (eval table exp2)
eval _ _ = Nothing

maybeEvalBinaire::(Double->Double->Double)->Maybe Double->Maybe Double->Maybe Double
maybeEvalBinaire f Nothing _ = Nothing
maybeEvalBinaire f _ Nothing = Nothing
maybeEvalBinaire f (Just op1) (Just op2) = Just(f op1 op2)

maybeEvalUnaire::(Double->Double)->Maybe Double->Maybe Double
maybeEvalUnaire f Nothing = Nothing
maybeEvalUnaire f (Just op) = Just(f op)




