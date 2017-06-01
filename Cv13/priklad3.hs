import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data Expr = Con Integer | Mul Expr Expr | Div Expr Expr 
            | Add Expr Expr | Sub Expr Expr
            deriving (Show)

def :: LanguageDef st    -- toto treba inak nesedi typ
def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "+-*/"
              , opLetter = oneOf "+-*/"
              , reservedOpNames = ["+", "-", "*", "/"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def
           
expr :: Parser Expr
expr = buildExpressionParser table factor <?> "expression"

table = [[op "*" Mul AssocLeft, op "/" Div AssocLeft]
        ,[op "+" Add AssocLeft, op "-" Sub AssocLeft]]
  where
    op s f assoc = Infix (m_reservedOp s >> return f) assoc

number :: Parser Integer
number = do{ ds <- many1 digit
             ; return (read ds)
           }
           <?> "number"

factor = m_parens expr
   <|> do {v <- number; m_whiteSpace; return (Con v)}
   <?> "simple expression" 

run :: Show a => Parser a -> String -> IO ()
run p input 
   = case (parse p "" input) of
           Left err -> do { putStr "parse error at "
                          ; print err
                          }
           Right x -> print x      
					 
-- run expr "12+3*4/2"    					 