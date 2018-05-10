-- priklad z https://wiki.haskell.org/Parsing_expressions_and_statements
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr   deriving Show
data Unop = Not deriving Show
data Duop = And | Iff | Or deriving Show
data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt | For String Expr Expr Expr Stmt| Seq [Stmt]  deriving Show
     
def :: LanguageDef st    -- toto treba inak nesedi typ
def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "~&=:|" 
              , opLetter = oneOf "~&=:|"
              , reservedOpNames = ["~", "&", "=", "||", ":="]
              , reservedNames = ["true", "false", "nop",
                                 "if", "then", "else", "fi",
                                 "while", "do", "od", "for"]
              }
              
TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def
           
exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "||" >> return (Duo Or)) AssocLeft]
        , [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> (m_reserved "true" >> return (Con True))
       <|> (m_reserved "false" >> return (Con False))

mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (m_semiSep1 stmt1)
      stmt1 = (m_reserved "nop" >> return Nop)
              <|> do { v <- m_identifier
                     ; m_reservedOp ":="
                     ; e <- exprparser
                     ; return (v := e)
                     }
              <|> do { m_reserved "if"
                     ; b <- exprparser
                     ; m_reserved "then"
                     ; p <- stmtparser
                     ; m_reserved "else"
                     ; q <- stmtparser
                     ; m_reserved "fi"
                     ; return (If b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- exprparser
                     ; m_reserved "do"
                     ; p <- stmtparser
                     ; m_reserved "od"
                     ; return (While b p)
                     }
              <|> do { m_reserved "for"
                     ; v <- m_identifier
                     ; m_reservedOp ":="
                     ; e1 <- exprparser
                     ; m_reservedOp ";"
                     ; e2 <- exprparser
                     ; m_reservedOp ";"
                     ; e3 <- exprparser
                     ; m_reserved "do"
                     ; p <- stmtparser
                     ; m_reserved "od"
                     ; return (For v e1 e2 e3 p)
                     }

run :: String -> IO ()
run inp = case parse mainparser "" inp of
              { Left err -> print err
              ; Right ans -> print ans
              }            

t1 = "if x = true then r := x else q := true fi"        

-- *Main> run t1
-- Seq [If (Duo Iff (Var "x") (Con True)) (Seq ["r" := Var "x"]) (Seq ["q" := Con True])]
-- dorobte forcyklus

t2 = "for i:=x; x&x; x do if x = true then r := x else q := true fi od"        
