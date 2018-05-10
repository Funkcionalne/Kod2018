-- aby sa to skompilovalo treba pridat do ghc prepinac -XFlexibleContexts
{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

def :: LanguageDef st    -- toto treba inak nesedi typ
def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "+-*/!"
              , opLetter = oneOf "+-*/!"
              , reservedOpNames = ["+", "-", "*", "/", "!"]
              }
TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
					 , natural = m_natural					 
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

expr :: Parser Integer
expr = buildExpressionParser table factor <?> "expression"

table = [[Postfix (do {char '!'; return (\n -> product [1..n])})]
         ,[Prefix (do {string "log";
                      return (\x -> ceiling ((log (fromIntegral x))/(log 2)))})]
         ,[op "*" (*) AssocLeft, op "/" div AssocLeft]
         ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]
        ]
  where
    op s f assoc = Infix (do{ string s; return f}) assoc

--factor 	:: Parser Integer		
factor =  m_parens expr
					<|> 			
					m_natural 
				  <?> "simple expression"
					
 
run :: Show a => Parser a -> String -> IO ()
run p input 
   = case (parse p "" input) of
           Left err -> do { putStr "parse error at "
                          ; print err
                          }
           Right x -> print x      
 
runLex :: Show a => Parser a -> String -> IO ()
runLex p input
    = run (do { m_whiteSpace
              ; x <- p
              ; eof
              ; return x
              }) input

-- priklad pouzitia 
-- run expr "12+3*4/2"    
-- dorobte postfixovy operator !, ktoreho vysledkom je faktorial, 
-- a infixovy operator log, ktoreho hodnotou je dolna/horna cela cast z
-- dvojkoveho logaritmu z cisla/argumentu
-- priklady
-- run expr "3!+5!"
-- run expr "log32+1"
 