-- aby sa to skompilovalo treba pridat do ghc prepinac -XFlexibleContexts
{-# LANGUAGE FlexibleContexts #-}
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

run  :: Show a => Parser a -> String -> IO()
run p input = case (parse p "" input) of
	  Left err -> do { putStr "parse error at" ; print err }	
	  Right x -> print x

-- V -> a V b | ε
parserW :: Parser ()
parserW = do { 
             char 'a'
             ; parserW
             ; char 'b'
             ; return ()
          } 
          <|> return ()

    
-- V -> a V b | ε
parserV :: Parser String
parserV = do { 
             char 'a'; x<-parserV; char 'b'; return ("a" ++ (x ++ "b"))
          } 
          <|> return ""

-- Q -> a Q a | ε
parserQ :: Parser String
parserQ = try ( do { 
             char 'a'; x<-parserQ; char 'a'; return ("a" ++ (x ++ "a"))
          } )
          <|> return ""
          
-- R -> a R a | b R b | a | b
parserR :: Parser String
parserR = try ( do { 
             char 'a'; x<-parserR; char 'a'; return ("a" ++ (x ++ "a"))
          } )
          <|> 
          try ( do { 
             char 'b'; x<-parserR; char 'b'; return ("b" ++ (x ++ "b"))
          } )
          <|> 
          do {
            char 'a'; return "a"
          }
          <|> 
          do {
            char 'b'; return "b"
          }
          
          
{-                  
--------------------
{-- zle riesenie}               
parserS :: Parser Char [Char]
parserS = ( parserS <*> parserS ) <@ (\(x,y) -> (x++y))
          <|>
          succeed []
---}

parserS :: Parser Char [Char]
parserS = ( symbol 'a' <*> parserS ) <@ (\(x,y) -> (x : y))
          <|>
          succeed []

          
justparserS :: Parser Char [Char]
justparserS = just $ parserS

--------------------
                
parserR :: Parser Char [Char]
parserR = (
            symbol 'a' <*> parserR <*> symbol 'a'
            <|>
            symbol 'b' <*> parserR <*> symbol 'b'
            <|>
            symbol 'c' <*> parserR <*> symbol 'c'
        ) <@ (\(x, (y,z)) -> (x:(y ++ [z])))
          <|>
          succeed []
                          
justparserR :: Parser Char [Char]
justparserR = just $ parserR
                          -}