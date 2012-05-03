{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Sonnet.Parser.AST where

import Text.InterpolatedString.Perl6

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Data.Monoid
import Data.String.Utils

import Sonnet.Parser.Utils



newtype Program = Program [Statement]

data Definition = Definition String [Axiom]

data Statement = TypeStatement TypeDefinition UnionType
               | DefinitionStatement Definition
               | ExpressionStatement Expression
               | ImportStatement Namespace
               | ModuleStatement Namespace [Statement]

newtype Namespace = Namespace [String] deriving (Eq)

instance Monoid Namespace where
    mempty = Namespace []
    mappend (Namespace x) (Namespace y) = Namespace (x ++ y)

data Axiom = TypeAxiom UnionType
           | EqualityAxiom Match Expression

data TypeDefinition = TypeDefinition String [String]

data Match = Match [Pattern] (Maybe Expression)

data Pattern = VarPattern String
             | AnyPattern
             | LiteralPattern Literal
             | RecordPattern (M.Map String Pattern)
             | ListPattern [Pattern]
             | ViewPattern Expression Pattern
             | NamedPattern String (Maybe Pattern)

data Expression = ApplyExpression Expression [Expression]
                | NamedExpression String (Maybe Expression)
                | IfExpression Expression Expression Expression
                | LiteralExpression Literal
                | SymbolExpression String
                | JSExpression String
                | FunctionExpression [Axiom]
                | RecordExpression (M.Map String Expression)
                | InheritExpression Expression (M.Map String Expression)
                | LetExpression [Definition] Expression
                | ListExpression [Expression]

data Literal = StringLiteral String | IntLiteral Int | DoubleLiteral Double

data UnionType = UnionType (S.Set ComplexType)
               deriving (Ord, Eq)

data ComplexType = RecordType (M.Map String UnionType)
                 | InheritType SimpleType (M.Map String UnionType)
                 | FunctionType UnionType UnionType
                 | SimpleType SimpleType
                 | NamedType String (Maybe UnionType)
                 deriving (Eq, Ord)

data SimpleType = PolymorphicType SimpleType [UnionType]
                | SymbolType String 
                | VariableType String
                deriving (Ord, Eq)

instance Show Program where
     show (Program ss) = sep_with "\n\n" ss

instance Show Namespace where
    show (Namespace []) = "global"
    show (Namespace x) = concat $ L.intersperse "." x

instance Show Statement where
    show (TypeStatement t c)     = [qq|type $t = $c|]
    show (DefinitionStatement d) = show d
    show (ExpressionStatement x) = show x
    show (ImportStatement x) = [qq|import $x|]
    show (ModuleStatement x xs) = replace "\n |" "\n     |" 
                                  $ replace "\n\n" "\n\n    " 
                                  $ "module " 
                                  ++ show x ++ "\n\n" ++ sep_with "\n\n" xs

instance Show Definition where
    show (Definition name ax) =[qq|$name {sep_with "\\n" ax}|]


instance Show Axiom where
    show (TypeAxiom x) = ": " ++ show x
    show (EqualityAxiom ps ex) = [qq| | $ps = $ex|]

instance Show TypeDefinition where
    show (TypeDefinition name vars) = concat . L.intersperse " " $ name : vars


-- Patterns
-- -----------------------------------------------------------------------------
-- TODO when patterns

instance Show Match where
    show (Match p Nothing)  = sep_with " " p
    show (Match p (Just x)) = [qq|{sep_with " " p} when $x|]

instance Show Pattern where
    show (VarPattern x)     = x
    show AnyPattern         = "_"
    show (LiteralPattern x) = show x
    show (ListPattern x)    = [qq|[ {sep_with ", " x} ]|]
    show (ViewPattern x y)  = [qq|($x -> $y)|]
    show (NamedPattern n (Just x)) = [qq|$n: ($x)|]
    show (NamedPattern n Nothing)  = n ++ ":"
    show (RecordPattern m)  = [qq|\{ {unsep_with " = " m} \}|] 

instance Show Expression where
    show (ApplyExpression x @ (SymbolExpression (f : _)) y) 
        | f `elem` "abcdefghijklmnopqrstuvwxyz" = [qq|$x {sep_with " " y}|]
        | length y == 2                         = [qq|{y !! 0} $x {y !! 1}|]
    show (ApplyExpression x y)        = [qq|$x {sep_with " " y}|]
    show (IfExpression a b c)         = [qq|if $a then $b else $c|]
    show (LiteralExpression x)        = show x
    show (SymbolExpression x)         = x
    show (ListExpression x)           = [qq|[ {sep_with ", " x} ]|]
    show (FunctionExpression as)      = replace "\n |" "\n     |" $ [qq|λ{sep_with "" as}|]
    show (NamedExpression n (Just x)) = [qq|$n: ($x)|]
    show (NamedExpression n Nothing)  = n ++ ":"
    show (JSExpression x)             = "`" ++ x ++ "`"
    show (LetExpression ax e)         = replace "\n |" "\n     |" $ [qq|let {sep_with "\\n" ax} in ($e)|]
    show (RecordExpression m)         = [qq|\{ {unsep_with " = " m} \}|] 
    show (InheritExpression x m)      = [qq|\{ $x with {unsep_with " = " m} \}|] 




-- Literals
-- -----------------------------------------------------------------------------
-- Literals in Sonnet are limited to strings and numbers - 


instance Show Literal where
   show (StringLiteral x) = show x
   show (IntLiteral x)    = show x
   show (DoubleLiteral x) = show x



-- Types

instance Show UnionType where 
    show (UnionType xs)         = [qq|{sep_with " | " $ S.toList xs}|]
   
instance Show ComplexType where
    show (SimpleType y)         = [qq|$y|]
    show (NamedType n (Just x)) = [qq|$n: ($x)|]
    show (NamedType n Nothing)  = n ++ ":"
    show (InheritType n m)      = [qq|\{ $n with {unsep_with ": " m} \}|]
    show (RecordType m)         = [qq|\{ {unsep_with ": " m} \}|] 

    show (FunctionType g@(UnionType (S.toList -> ((FunctionType _ _):[]))) h) = [qq|($g -> $h)|]
    show (FunctionType g h)     = [qq|$g -> $h|]

instance Show SimpleType where
    show (PolymorphicType x y)  = [qq|($x {sep_with " " y})|]
    show (SymbolType x)   = x
    show (VariableType x) = x

