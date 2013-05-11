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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}

module Forml.Types.Definition where

import Text.InterpolatedString.Perl6
import Language.Javascript.JMacro

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Serialize

import Text.Parsec hiding ((<|>), State, many, spaces, parse, label)
import qualified Text.Parsec as P
import Text.Parsec.Indent  hiding (same)

import Forml.Parser.Utils
import Forml.Javascript.Utils
import Forml.Javascript.Backend

import Forml.Types.Type
import Forml.Types.Symbol
import Forml.Types.Pattern
import Forml.Types.Axiom
import Forml.Types.Expression

import Prelude hiding (curry, (++))

import GHC.Generics



-- Definition
-- --------------------------------------------------------------------------------

data Visibility = Public | Private deriving (Eq, Generic)

data Definition = Definition Visibility Bool Symbol [Axiom (Expression Definition)] deriving (Eq, Generic)

instance Serialize Definition
instance Serialize Visibility

instance Show Definition where
    show (Definition Public _ name ax) =[qq|$name {sep_with "\\n" ax}|]
    show (Definition Private _ name ax) =[qq|private $name {sep_with "\\n" ax}|]

instance Syntax Definition where

    syntax = do whitespace
                vis <- option Public (try (string "private" >> spaces >> return Private))
                inl <- option False (try (string "inline" >> spaces >> return True))
                (x, y) <- try prefix <|> try infix''
                return $ Definition vis inl x y

        where where' = do
                  string "where"
                  whitespace1
                  P.spaces
                  Just <$> withPosTemp (syntax `sepBy1` (try comma <|> try (spaces *> same)))

              where_clause ex = do
                  where'' <- option Nothing (try where')
                  return $ case where'' of
                      Just defs -> LetExpression defs ex
                      Nothing -> ex

              infix'' =

                  do whitespace
                     first_arg <- try syntax
                     whitespace
                     op <- not_reserved $ many1 operator
                     whitespace
                     Match pss cond <- syntax
                     ax <- no_args_eq_axiom $ Match (first_arg:pss) cond
                     axs <- try (prefix' $ Operator op) <|> return []
                     return (Operator op, ax:axs)

                  where no_args_eq_axiom patterns =

                          do whitespace *> string "=" *> spaces *> indented
                             (Addr a b ex') <- withPosTemp (addr syntax)
                             P.spaces
                             ex <- where_clause ex'
                             return $ EqualityAxiom patterns (Addr a b ex)

              prefix = 

                  do name <- try syntax <|> (Symbol <$> many1 (char '_'))
                     y <- prefix' name
                     return (name, y)
          
              prefix' name =

                       do sig <- first
                          eqs <- (try $ spaces *> (withPos . many . try $ try infix_axiom <|> eq_axiom)) <|> return []
                          whitespace
                          if length sig == 0 && length eqs == 0 
                              then parserFail "Definition Axioms"
                              else return (sig ++ eqs)

                where first = try type_or_first
                              <|> ((:[]) <$> try naked_eq_axiom)
                              <|> ((:[]) <$> (whitespace *> infix_axiom))
                              <|> return []

                      type_or_first = (:) <$> type_axiom <*> second

                      second = option [] ((:[]) <$> try (no_args_eq_axiom (Match [] Nothing)))

                      eq_axiom =

                          do P.spaces
                             string "|" <|> try (string name' <* notFollowedBy (digit <|> letter)) 
                             naked_eq_axiom

                          where name' = case name of
                                         (Symbol name'')   -> name''
                                         (Operator name'') -> "(" ++ name'' ++ ")"

                      naked_eq_axiom =

                          do whitespace
                             patterns <- syntax
                             no_args_eq_axiom patterns

                      no_args_eq_axiom patterns =

                          do P.spaces *> string "=" *> P.spaces
                             (Addr a b ex') <- withPosTemp (addr syntax)
                             P.spaces
                             ex <- where_clause ex'
                             return $ EqualityAxiom patterns (Addr a b ex)

                      infix_axiom =
                          
                          case name of
                             (Symbol _)   -> parserFail "Infix Definition"
                             (Operator name'') -> do first_arg <- syntax
                                                     whitespace
                                                     string name''
                                                     whitespace
                                                     Match pss cond <- syntax
                                                     no_args_eq_axiom $ Match (first_arg:pss) cond 

                      type_axiom =

                          do spaces
                             indented
                             string ":"
                             spaces
                             indented
                             TypeAxiom <$> withPos type_axiom_signature

-- TODO Visibility should be more than skin deep?

instance Javascript Definition JStat where
    toJS (Definition _ _ _ (TypeAxiom _: [])) =
        return mempty
    toJS (Definition _ _ name as) =
        return [jmacro| `(declare_this (to_name name) $ toJExpr as)`; |]

instance ToLocalStat Definition where
    toLocal (Definition _ _ _ (TypeAxiom _: [])) =
        mempty
    toLocal (Definition _ _ name as) =
        [jmacro| `(declare (to_name name) $ toJExpr as)`; |]
