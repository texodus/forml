{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Formal.Parser where

import Control.Applicative
import Data.String.Utils
import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import Formal.Parser.Utils

import Formal.Types.Statement

-- Parsing 
-- -----------------------------------------------------------------------------
-- A Sonnet program is represented by a set of statements

parseFormal :: String -> Either ParseError Program
parseFormal src = case parse ((comment <|> return "\n") `manyTill` eof) "Cleaning comments" src of 
                    Right x -> parse syntax "parsing syntax" (concat x)


compress :: String -> String
compress = run var . run nul . run fun

    where run x src' = case parse ((try x <|> ((:[]) <$> anyChar)) `manyTill` eof) "Compressing" src' of
                         Right z -> concat z

          var = do string "var"
                   spaces
                   name <- many1 (alphaNum <|> char '_')
                   string ";"
                   spaces
                   string name
                   return $ "var " ++ name

          nul = do string "var"
                   spaces
                   name <- many1 (alphaNum <|> char '_')
                   string ";"
                   spaces
                   string name
                   spaces
                   string "="
                   spaces
                   string "null"
                   return $ "var " ++ name  

          pairs = do string "{"
                     inner <- (try pairs <|> ((:[]) <$> anyChar)) `manyTill` string "}"
                     return $ "{" ++ concat inner ++ "}"

          fun = do string "(function()"
                   spaces
                   string "{"
                   spaces
                   string "return"
                   spaces
                   string "(function("
                   name <- many1 (alphaNum <|> char '_')
                   string ")"
                   spaces
                   content <- pairs
                   spaces
                   string ");"
                   spaces
                   string "})()"
                   return $ "(function(" ++ name ++ ")" ++ content ++ ")"

get_tests :: [Statement] -> [(SourcePos, SourcePos)]
get_tests [] = []
get_tests (ExpressionStatement x _: xs) = x : get_tests xs
get_tests (ModuleStatement _  x: xs) = get_tests x ++ get_tests xs
get_tests (_: xs) = get_tests xs

annotate_tests :: String -> Program -> String
annotate_tests zz (Program xs) = annotate_tests' zz (get_tests xs)
    where annotate_tests' x [] = x
          annotate_tests' x ((a, b):ys) = 
              annotate_tests' marked ys
                  where marked = mark (mark x (serial a b ++ "--") row' col') ("--" ++ serial a b) row col

                        mark str tk x y =
                            let str' = lines str
                            in  unlines $ take x str' 
                                    ++ [take y (str' !! x) ++ tk ++ drop y (str' !! x)]
                                    ++ drop (x + 1) str'

                        row = sourceLine a - 1
                        col = sourceColumn a - 1
                    
                        row' = sourceLine b - 1
                        col' = sourceColumn b +1

          
highlight :: [(SourcePos, SourcePos)] -> String -> String                              
highlight [] x = x
highlight ((a, b):xs) y = highlight xs (replace ("--" ++ serial a b) ("<span class='test' id='test_" ++ serial a b ++ "'>") (replace (serial a b ++ "--") "</span>" y))


newtype Program = Program [Statement]

instance Show Program where
     show (Program ss) = sep_with "\n\n" ss

instance Syntax Program where
    syntax = Program <$> many (many (string "\n") >> syntax) <* eof
