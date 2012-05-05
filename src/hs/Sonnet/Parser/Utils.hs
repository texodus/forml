{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Sonnet.Parser.Utils where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State hiding (lift)

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import Text.Parsec.Indent  hiding (same)

import qualified Data.Map as M
import qualified Data.List as L

import Data.String.Utils

type Parser a = ParsecT String () (StateT SourcePos Identity) a

parse :: Parser a -> SourceName -> String -> Either ParseError a
parse parser sname input = runIndent sname $ runParserT parser () sname input

whitespace :: Parser String
whitespace = many $ oneOf "\t "

whitespace1 :: Parser String
whitespace1 = space >> whitespace

same :: Parser ()
same = spaces >> do pos <- getPosition
                    s <- get
                    if (sourceColumn pos) /= (sourceColumn s) 
                       then parserFail "not indented" 
                       else do put $ setSourceLine s (sourceLine pos)
                               return ()

spaces :: Parser ()
spaces = try emptyline `manyTill` try line_start >> return ()

    where emptyline  = whitespace >> newline
          line_start = whitespace >> notFollowedBy newline >> in_block

          in_block = do pos <- getPosition
                        s <- get
                        if (sourceColumn pos) < (sourceColumn s) 
                           then parserFail "not indented" 
                           else do put $ setSourceLine s (sourceLine pos)
                                   return ()


comment :: Parser String
comment = try empty_line <|> try commented_code <|> try code <|> markdown_comment

    where markdown_comment = anyChar `manyTill` newline *> return "\n"
          empty_line = whitespace *> newline *> return "\n"
          code = (\x y -> x ++ rstrip y ++ "\n") <$> string "    " <*> (anyChar `manyTill` newline)
          commented_code = do string "    "
                              x <- (noneOf "\n") `manyTill` string "--"
                              anyChar `manyTill` newline
                              return $ if length (strip x) > 0 
                                           then "    " ++ (rstrip x) ++ "\n" 
                                           else "\n"

sep_with :: Show a => String -> [a] -> String
sep_with x = concat . L.intersperse x . fmap show

unsep_with :: forall a b. (Show a, Show b) => String -> (M.Map b a) -> String
unsep_with z = concat . L.intersperse ", " . fmap (\(x, y) -> concat [show x, z, show y]) . M.toAscList

(<:>) :: Parser a -> Parser [a] -> Parser [a]
(<:>) x y = (:) <$> x <*> y

operator_dict :: M.Map Char String
operator_dict = M.fromList [ ('!', "_bang"), 
                             ('@', "_at"),
                             ('#', "_hash"),
                             ('$', "$"), 
                             ('%', "_perc"), 
                             ('^', "_exp"),
                             ('&', "_and"),
                             ('|', "_or"),
                             ('<', "_leff"),
                             ('>', "_grea"), 
                             ('?', "_ques"), 
                             ('/', "_forw"),
                             ('=', "_eq"), 
                             ('\\', "_back"), 
                             ('~', "_tild"),
                             ('+', "_plus"),
                             ('-', "_minu"),
                             (',', "_comm"),
                             ('.', "_comp") ]

operator :: Parser Char
operator = oneOf (fst . unzip . M.toList $ operator_dict)

not_reserved :: Parser String -> Parser String
not_reserved x = do y <- x
                    if y `elem` reserved_words
                       then parserFail "non-reserved word"
                       else return y

    where reserved_words = [ "if", "then", "else", "type", "let", "when", "with", "and", "or", "do",
                             "module", "open", "import",
                             "|", "\\", "=", ".", ":", ",", "==", "-", "->", "<=", ">=", "<", ">", "<-" ]

type_sep    :: Parser Char
indentPairs :: String -> Parser a -> String -> Parser a
not_comma   :: Parser ()
comma       :: Parser ()

type_sep          = try (spaces *> char '|' <* whitespace)
indentPairs a p b = string a *> spaces *> withPos p <* spaces <* string b
not_comma         = whitespace >> newline >> spaces >> notFollowedBy (string "}")
comma             = spaces *> string "," *> spaces
