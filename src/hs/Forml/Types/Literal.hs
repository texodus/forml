{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}


module Forml.Types.Literal where

import Language.Javascript.JMacro

import Control.Applicative
import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)

import Forml.Parser.Utils
import Forml.TypeCheck.Types

import GHC.Generics
import Data.Serialize

data Literal = StringLiteral String | IntLiteral Int | DoubleLiteral Double deriving (Eq, Generic)

instance Serialize Literal

instance Show Literal where
   show (StringLiteral x) = show x
   show (IntLiteral x)    = show x
   show (DoubleLiteral x) = show x

instance Syntax Literal where

    syntax = try flt <|> try num <|> try str

        where flt = DoubleLiteral <$> syntax 
              num = IntLiteral    <$> syntax
              str = StringLiteral <$> syntax

instance ToJExpr Literal where
    toJExpr (StringLiteral s) = toJExpr s
    toJExpr (IntLiteral s)    = toJExpr s
    toJExpr (DoubleLiteral s) = toJExpr s

instance Infer Literal Type where
    infer (StringLiteral _) = return (Type (TypeConst "String" Star))
    infer (IntLiteral _)    = return (Type (TypeConst "Num" Star))
    infer (DoubleLiteral _)    = return (Type (TypeConst "Num" Star))
    