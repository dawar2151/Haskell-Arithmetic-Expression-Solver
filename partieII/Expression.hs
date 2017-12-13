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

-- Type Binary Operation
data BinaryOperator = 
     Addition
     |Multiplication
     |Soustraction
     |Division
     deriving (Show, Eq)
     
-- Type Unary Operation
data UnaryOperator = 
 Neg
 |Sin
 |Cos
 deriving (Show, Eq)  

-- Type recursif Expression
data Expression=
 Literal Double
 | Variable String
 | UnaryOperation UnaryOperator Expression
 | BinaryOperation BinaryOperator Expression Expression
 deriving (Show, Eq)

-- Eval Uniary and Binary Operation (Maybe)
eval :: Store -> Expression -> Maybe Double
eval table (Literal n) = Just n
eval table (UnaryOperation Sin exp)  = maybeEvalUnaire (sin) (eval table exp)
eval table (UnaryOperation Cos exp)  = maybeEvalUnaire (cos) (eval table exp)
eval table (BinaryOperation Division exp1 exp2)  = maybeEvalBinaire (/) (eval table exp1) (eval table exp2)
eval _ _ = Nothing

-- Eval Binary Operation (Double)
maybeEvalBinaire::(Double->Double->Double)->Maybe Double->Maybe Double->Maybe Double
maybeEvalBinaire f Nothing _ = Nothing
maybeEvalBinaire f _ Nothing = Nothing
maybeEvalBinaire f (Just op1) (Just op2) = Just(f op1 op2)

-- Eval  Unary Operation (Double)
maybeEvalUnaire::(Double->Double)->Maybe Double->Maybe Double
maybeEvalUnaire f Nothing = Nothing
maybeEvalUnaire f (Just op) = Just(f op)




