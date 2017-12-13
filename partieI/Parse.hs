  module Parse(
    parseExpression
  ) where
  import Control.Monad
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token
  import Expression

  aExpression :: Parser Expression
  aExpression = buildExpressionParser aOperators aTerm

  aTerm = parens aExpression
    <|> liftM Variable identifier
    <|> liftM (Literal . fromIntegral) integer
    <|> liftM Literal float
    
  parseExp :: String -> Either ParseError Expression
  parseExp = parse aExpression ""

  parseExpression :: String -> Maybe Expression
  parseExpression s = case (parseExp s) of
    Right xs -> Just xs
    Left error -> Nothing

  aOperators = [
   [Prefix (reservedOp "-"   >> return (UnaryOperation Neg))],
   [Infix  (reservedOp "*"   >> return (BinaryOperation Multiplication)) AssocLeft,
   Infix  (reservedOp "/"   >> return (BinaryOperation Division  )) AssocLeft],
   [Infix  (reservedOp "+"   >> return (BinaryOperation Addition    )) AssocLeft,
   Infix  (reservedOp "-"   >> return (BinaryOperation Soustraction)) AssocLeft]
   ]

  identifier = Token.identifier lexer -- parses an identifier
  reserved   = Token.reserved   lexer -- parses a reserved name
  reservedOp = Token.reservedOp lexer -- parses an operator
  parens     = Token.parens     lexer -- parses surrounding parenthesis:
  float    = Token.float   lexer -- parses an integer
  integer    = Token.integer   lexer -- parses an integer
  semi       = Token.semi       lexer -- parses a semicolon
  whiteSpace = Token.whiteSpace lexer -- parses whitespace
  languageDef =
     emptyDef { Token.commentStart    = "/*"
              , Token.commentEnd      = "*/"
              , Token.commentLine     = "//"
              , Token.identStart      = letter
              , Token.identLetter     = alphaNum
              , Token.reservedNames   = [ "if"
                                        , "then"
                                        , "else"
                                        , "while"
                                        , "do"
                                        , "skip"
                                        , "true"
                                        , "false"
                                        , "not"
                                        , "and"
                                        , "or"
                                        ]
              , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                        , "<", ">", "and", "or", "not"
                                        ]
              }
  lexer = Token.makeTokenParser languageDef