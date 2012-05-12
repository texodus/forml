{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-} 

module Formal.Types.TypeDefinition where
import Text.InterpolatedString.Perl6


import Language.Javascript.JMacro

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (lift)

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import Text.Parsec.Indent  hiding (same)
import Text.Parsec.Expr

import Data.String.Utils hiding (join)
import qualified Data.Map as M
import qualified Data.List as L

import Formal.Parser.Utils

import Formal.Types.Type
import Formal.Types.Literal
import Formal.Types.Symbol


data TypeDefinition = TypeDefinition String [String]


instance Show TypeDefinition where
    show (TypeDefinition name vars) = concat . L.intersperse " " $ name : vars


type_definition :: Parser TypeDefinition

type_definition = do name <- (:) <$> upper <*> many alphaNum
                     vars <- try vars' <|> return []
                     return $ TypeDefinition name vars

    where vars' = do many1 $ oneOf "\t "
                     let var = (:) <$> lower <*> many alphaNum
                     var `sepEndBy` whitespace
