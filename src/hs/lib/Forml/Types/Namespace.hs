{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Forml.Types.Namespace where

import Language.Javascript.JMacro

import Control.Applicative

import Text.Parsec hiding ((<|>), State, many, spaces, parse, label)
import Data.Monoid
import qualified Data.List as L
import Data.Serialize

import GHC.Generics

import Forml.Parser.Utils
import Forml.Javascript.Utils

import Prelude hiding (curry, (++))

newtype Namespace = Namespace [String] deriving (Eq, Ord, Generic)

instance Serialize Namespace

instance Monoid Namespace where
    mempty = Namespace []
    mappend (Namespace x) (Namespace y) = Namespace (x ++ y)

instance Show Namespace where
    show (Namespace []) = "global"
    show (Namespace x) = concat $ L.intersperse "." x

instance Syntax Namespace where
    syntax = Namespace <$> (many1 (lower <|> char '_') `sepBy1` char '.')

instance ToJExpr Namespace where
    toJExpr (Namespace []) = [jmacroE| (typeof global == "undefined" ? window : global) |]
    toJExpr (Namespace (end -> x : xs)) = [jmacroE| `(Namespace xs)`["$" ++ `(x)`] |]

data Module = Module Namespace [Module]
            | Var String

instance Show Module where
    show (Module (Namespace (reverse -> (x:_))) _) = x
    show (Var s) = s

