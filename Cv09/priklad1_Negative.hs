-- aby sa to skompilovalo treba pridat do ghc prepinac -XFlexibleContexts
{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

expr :: Parser Integer
expr = buildExpressionParser table factor <?> "expression"

table = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
        ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]]
  where
    op s f assoc = Infix (do{ string s; return f}) assoc

factor = do{ char '('
             ; x <- expr
             ; char ')'
             ; return x
           }
          <|> number
          <?> "simple expression"

number :: Parser Integer
number = do{ ds <- many1 digit
             ; return (read ds)
           }
           <|>
           do{ char '-'; ds <- many1 digit
             ; return (-(read ds))
           }
           <?> "number"

run :: Show a => Parser a -> String -> IO ()
run p input 
   = case (parse p "" input) of
           Left err -> do{ putStr "parse error at "
                           ; print err
                         }
           Right x -> print x           
           
-- priklad pouzitia !!! vo vyraze nemozu byt medzery
-- run expr "1+1"       

-- run expr "1+-2"
    