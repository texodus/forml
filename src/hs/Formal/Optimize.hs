
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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Formal.Optimize where
import System.IO.Unsafe

import Data.List (nub, (\\), intersect, union, partition)

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Char

import Text.ParserCombinators.Parsec

import Formal.Types.Literal
import Formal.Types.Pattern
import Formal.Types.Symbol
import Formal.Types.Expression
import Formal.Types.Definition
import Formal.Types.Axiom
import Formal.Types.Statement hiding (find, namespace, modules, Test)
import Formal.Types.Namespace hiding (Module)

import Formal.Types.TypeDefinition
import Formal.Types.Type

import Formal.TypeCheck hiding (get_namespace)

import Formal.Parser.Utils
import Formal.Parser


data OptimizeState = OptimizeState { ns :: Namespace
                                   , assumptions :: M.Map Namespace [Assumption]
                                   , inlines :: M.Map (Namespace, Symbol) (Expression Definition) }

data Optimizer a = Optimizer (OptimizeState -> (OptimizeState, a))

instance Monad Optimizer where

    fail   x = Optimizer (\y -> error x)
    return x = Optimizer (\y -> (y, x))

    Optimizer f >>= g =
        Optimizer (\x -> case f x of (y, x) -> let Optimizer gx = g x in gx y)

instance Functor Optimizer where

    fmap f (Optimizer g) = Optimizer (\x -> case g x of (y, x) -> (y, f x))

class Optimize a where

    optimize :: a -> Optimizer a

set_namespace :: Namespace -> Optimizer ()
set_namespace ns' = Optimizer (\x -> (x { ns = ns' }, ()))

get_namespace :: Optimizer Namespace
get_namespace  = Optimizer (\x -> (x, ns x))

instance Optimize Definition where

    optimize (Definition _ True name [EqualityAxiom m ex]) = undefined

instance Optimize (Expression Definition) where

    optimize x = undefined

instance Optimize Statement where

    optimize (DefinitionStatement d) = DefinitionStatement <$> optimize d
    optimize (ExpressionStatement (Addr s e x)) = ExpressionStatement . Addr s e <$> optimize x
    optimize (ModuleStatement x xs) =

        do ns <- get_namespace
           set_namespace$ ns `mappend` x
           xs' <- optimize xs
           set_namespace ns
           return$ ModuleStatement x xs'
                                         
    optimize x = return x

instance Optimize [Statement] where

    optimize (x:xs) = optimize x >> optimize xs

instance Optimize Program where

    optimize (Program xs) = Program <$> optimize xs

run_optimizer :: Program -> M.Map Namespace [Assumption] -> Program
run_optimizer (optimize -> Optimizer f) as = case f gen_state of (_, p) -> p

    where gen_state = OptimizeState (Namespace []) as M.empty
                       