import Mparser

-- V -> a V b | ε
parserW :: Parser ()
parserW = do { 
             char 'a'
             ; parserW
             ; char 'b'
             ; return ()
          } 
          `plus` return ()
-- parse parserW "ab"
    
-- V -> a V b | ε
parserV :: Parser String
parserV = do { 
             char 'a'; x<-parserV; char 'b'; return ("a" ++ (x ++ "b"))
          } 
          +++ return ""
-- parse parserV "aabb"
-- parse (just parserV) "aabb"
          
-- Q -> a Q a | ε
parserQ :: Parser String
parserQ = do { 
             char 'a'; x<-parserQ; char 'a'; return ("a" ++ (x ++ "a"))
          }
          +++ return ""

-- Q -> a Q a | ε
parserQ' :: Parser String
parserQ' = do { 
             char 'a'; x<-parserQ'; char 'a'; return ("a" ++ (x ++ "a"))
          }
          `plus` return ""
          
          
-- R -> a R a | b R b | a | b
parserR :: Parser String
parserR = do { 
             char 'a'; x<-parserR; char 'a'; return ("a" ++ (x ++ "a"))
          }
          `plus` 
          do { 
             char 'b'; x<-parserR; char 'b'; return ("b" ++ (x ++ "b"))
          }
          `plus` 
          do {
            char 'a'; return "a"
          }
          `plus` 
          do {
            char 'b'; return "b"
          }
-- parse (just parserR) "ababa"          
          
just :: Parser a -> Parser a
just p = Parser (\xs -> [(v, xs') | (v, xs') <- (parse p) xs, xs' == [] ])