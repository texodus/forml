{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Sonnet.Parser (parseSonnet) where

import Control.Applicative

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import Text.Parsec.Indent  hiding (same)

import Sonnet.Parser.Utils
import Sonnet.Parser.Statements
import Sonnet.AST



-- Parsing 
-- -----------------------------------------------------------------------------
-- A Sonnet program is represented by a set of statements

parseSonnet :: String -> Either ParseError Program
parseSonnet src = case parse ((comment <|> return "\n") `manyTill` eof) "Cleaning comments" src of 
                    Right x -> parse sonnetParser "parsing syntax" (concat x)


sonnetParser :: Parser Program
sonnetParser  = Program . concat <$> many (many (string "\n") >> statement) <* eof

    where statement       = whitespace >> withPos statement_types <* many newline
          statement_types = (try type_statement       <?> "Type Definition")
                            <|> (try import_statement <?> "Import Statement")
                            <|> (try module_statement <?> "Module Declaration")
                            <|> (try def_statement    <?> "Symbol Definition")
                            <|> (expression_statement <?> "Assertion")

          def_statement = map DefinitionStatement <$> definition_statement

          import_statement = do string "open"
                                whitespace
                                (:[]) . ImportStatement . Namespace <$> namespace

          module_statement = do string "module"
                                whitespace1
                                name <- Namespace <$> namespace
                                spaces *> indented
                                (:[]) . ModuleStatement name . concat 
                                    <$> withPos (many1 (many (string "\n") >> same >> try statement))

          namespace = many1 lower `sepBy1` char '.'

