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



import Control.Applicative

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import qualified Data.List as L

import Formal.Parser.Utils


data TypeDefinition = TypeDefinition String [String]


instance Show TypeDefinition where
    show (TypeDefinition name vars) = concat . L.intersperse " " $ name : vars


instance Syntax TypeDefinition where

    syntax = do name <- (:) <$> upper <*> many alphaNum
                vars <- try vars' <|> return []
                return $ TypeDefinition name vars

        where vars' = do many1 $ oneOf "\t "
                         let var = (:) <$> lower <*> many alphaNum
                         var `sepEndBy` whitespace
