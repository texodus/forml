{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}

module Forml.Types.TypeDefinition where



import           Control.Applicative

import qualified Data.List           as L
import           Text.Parsec         hiding (State, label, many, parse, spaces,
                                      (<|>))

import           Forml.Parser.Utils


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
