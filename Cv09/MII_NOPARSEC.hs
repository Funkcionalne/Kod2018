module Cvicenie09 where

import Mparser

-- V -> a V b | ε
parserW :: Parser ()
parserW = do {
            char 'a';
            parserW;
            char 'b';
            return ()
          } `plus` do {
            return ()
          }
-- parse parserW "ab"
-- parse parserW "aabb"
-- parse (just parserW) "aabb"

just :: Parser a -> Parser a
just (Parser f) = Parser (\xs -> filter (\(_,b) -> b == []) (f xs))

-- V -> a V b | ε
parserV :: Parser String
parserV = do {
            char 'a';
            x <- parserV;
            char 'b';
            return ("a" ++ x ++ "b")
          } `plus` do {
            return ""
          }

-- parse parserV "aabb"
-- parse (just parserV) "aabb"
-- parse parserV "aabbba"
-- parse (just parserV) "aabbba"
          
-- Q -> a Q a | ε
parserQ :: Parser String
parserQ = do {
            char 'a';
            x <- parserQ;
            char 'a';
            return ( "a" ++ x ++ "a")
          } `plus` (return "")

-- Q -> a Q a | ε
parserQ' :: Parser String
parserQ' =  do {
            char 'a';
            x <- parserQ;
            char 'a';
            return ( "a" ++ x ++ "a")
          } +++ (return "")
          
-- R -> a R a | b R b | a | b
parserR :: Parser String
parserR = do {
            char 'a';
            x <- parserR;
            char 'a';
            return ( "a" ++ x ++ "a")
          } +++ do {
            char 'b';
            x <- parserR;
            char 'b';
            return ( "b" ++ x ++ "b")
          } +++ do {
            char 'a';
            return "a"
          } +++ do {
            char 'b';
            return "b"
          }

-- parse (just parserR) "ababa"          
          
