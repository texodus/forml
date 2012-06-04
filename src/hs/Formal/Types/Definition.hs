{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Formal.Types.Definition where

import Text.InterpolatedString.Perl6
import Language.Javascript.JMacro

import Control.Applicative
import Control.Monad

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import Text.Parsec.Indent  hiding (same)

import Formal.Parser.Utils
import Formal.Javascript.Utils

import Formal.Types.Type
import Formal.Types.Symbol
import Formal.Types.Pattern
import Formal.Types.Axiom
import Formal.Types.Expression

import Prelude hiding (curry, (++))



-- Definition
-- --------------------------------------------------------------------------------

data Definition = Definition Symbol [Axiom (Expression Definition)]

instance Show Definition where
    show (Definition name ax) =[qq|$name {sep_with "\\n" ax}|]

instance Syntax Definition where

    syntax = do whitespace
                name <- try syntax <|> (Symbol <$> many1 (char '_'))
                sig <- first
                eqs <- (try $ spaces *> (withPos . many . try $ eq_axiom)) <|> return []
                whitespace
                if length sig == 0 && length eqs == 0 
                    then parserFail "Definition Axioms"
                    else return $ Definition name (sig ++ eqs)

        where first = try type_or_first
                      <|> ((:[]) <$> try naked_eq_axiom) 
                      <|> return []

              type_or_first = (:) <$> type_axiom <*> second

              second = option [] ((:[]) <$> try (no_args_eq_axiom (Match [] Nothing)))

              eq_axiom   = do try (spaces >> same) <|> (whitespace >> return ())
                              string "|"
                              naked_eq_axiom

              naked_eq_axiom = do whitespace
                                  patterns <- syntax
                                  no_args_eq_axiom patterns

              no_args_eq_axiom patterns = do whitespace *> string "=" *> spaces *> indented
                                             ex <- withPos (addr syntax)
                                             return $ EqualityAxiom patterns ex

              type_axiom = do spaces
                              indented
                              string ":"
                              spaces
                              indented
                              TypeAxiom <$> withPos type_axiom_signature

instance ToStat Definition where
    toStat (Definition name as) = declare_this (to_name name) $ toJExpr as

instance ToLocalStat Definition where
    toLocal (Definition name as) = declare (to_name name) $ toJExpr as
