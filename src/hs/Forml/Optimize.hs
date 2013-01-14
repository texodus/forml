
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE DeriveGeneric          #-}

module Forml.Optimize where
import System.IO.Unsafe ()

import Control.Applicative
import Control.Monad

import qualified Data.Map as M
import qualified Data.List as L

import Data.Char
import Data.Monoid
import Data.Serialize

import Language.Javascript.JMacro

import Forml.Types.Axiom
import Forml.Types.Definition
import Forml.Types.Expression
import Forml.Types.Namespace  hiding (Module)
import Forml.Types.Pattern
import Forml.Types.Statement  hiding (Test, find, modules, namespace)
import Forml.Types.Symbol
import Forml.Javascript.Utils hiding ((++))
import Forml.Deps
import Forml.TypeCheck.Types hiding (get_namespace)
import Forml.Parser
import Forml.Parser.Utils
import Forml.Optimize.TailCall
import Forml.Optimize.Inline
import qualified Forml.Javascript.Utils as J

import Prelude hiding (curry)
import Text.Parsec.Pos (newPos)

import GHC.Generics

data OptimizeState = OptimizeState { ns :: Namespace
                                   , assumptions :: [(Namespace, [Assumption])]
                                   , inlines :: Inlines
                                   , tco :: [String]
                                   , env :: Inline } deriving (Eq, Generic)

data Optimizer a = Optimizer (OptimizeState -> (OptimizeState, a))

instance Serialize OptimizeState

instance Monad Optimizer where

    fail   x = Optimizer (\y -> error x)
    return x = Optimizer (\y -> (y, x))

    Optimizer f >>= g =
        Optimizer (\x -> case f x of (y, x) -> let Optimizer gx = g x in gx y)

instance Functor Optimizer where

    fmap f (Optimizer g) = Optimizer (\x -> case g x of (y, x) -> (y, f x))

instance Applicative Optimizer where

    pure = return
    x <*> y = do f <- x
                 f <$> y

class Optimize a where

    optimize :: a -> Optimizer a

set_namespace :: Namespace -> Optimizer ()
set_namespace ns' = Optimizer (\x -> (x { ns = ns' }, ()))

get_namespace :: Optimizer Namespace
get_namespace  = Optimizer (\x -> (x, ns x))

set_inline :: Inlines -> Optimizer ()
set_inline ns' = Optimizer (\x -> (x { inlines = ns' }, ()))

get_inline :: Optimizer Inlines
get_inline  = Optimizer (\x -> (x, inlines x))

set_env :: Inline -> Optimizer ()
set_env ns' = Optimizer (\x -> (x { env = ns' }, ()))

get_env :: Optimizer Inline
get_env  = Optimizer (\x -> (x, env x))

add_tco :: String -> Optimizer ()
add_tco x = Optimizer (\y -> (y { tco = x : tco y }, ()))

with_env :: forall b. Optimizer b -> Optimizer b
with_env xs =

    do e <- get_env
       xs' <- xs
       set_env e
       return xs'

instance (Optimize a) => Optimize (Maybe a) where

    optimize (Just x) = Just <$> optimize x
    optimize Nothing  = return Nothing

instance (Optimize a) => Optimize (Addr a) where

    optimize (Addr s e a) = Addr s e <$> optimize a

instance Optimize (Expression Definition) where

    optimize (ApplyExpression (ApplyExpression a b) c) =
        optimize (ApplyExpression a (b ++ c))

    optimize (ApplyExpression (SymbolExpression s) args) = do

        is <- get_env
        case (InlineSymbol s) `lookup` is of
            Just ((Match pss _), ex) | length pss == length args -> do
                args' <- mapM optimize args
                optimize $ inline_apply pss args args' ex
            _ -> ApplyExpression <$> optimize (SymbolExpression s) <*> mapM optimize args

    optimize (ApplyExpression f' args ) =
        ApplyExpression <$> optimize f' <*> mapM optimize args

    optimize a @ (AccessorExpression x xs) =

        do is <- get_env
           case (InlineRecord a) `lookup` is of
             Just (m @ (Match pss _), ex) ->

                 do ex'   <- optimize ex
                    m'    <- optimize m
                    return $ FunctionExpression [EqualityAxiom m' (Addr undefined undefined ex')]

             _ -> flip AccessorExpression xs <$> optimize x

    optimize (SymbolExpression f) =

        do is <- get_env
           case (InlineSymbol f) `lookup` is of
             Just (m @ (Match pss _), ex) ->

                 do ex'   <- optimize ex
                    m'    <- optimize m
                    return $ FunctionExpression [EqualityAxiom m' (Addr undefined undefined ex')]

             _ -> return $ SymbolExpression f

    optimize (ApplyExpression f args) = ApplyExpression <$> optimize f <*> mapM optimize args
    optimize (IfExpression a b c) = IfExpression <$> optimize a <*> optimize b <*> optimize c
    optimize (LazyExpression x l) = flip LazyExpression l <$> optimize x
    optimize (FunctionExpression xs) = FunctionExpression <$> mapM optimize xs
    optimize (ListExpression ex) = ListExpression <$> mapM optimize ex

    optimize (LetExpression ds ex) = do

        stmts' <- mapM optimize (sorted_defs . map DefinitionStatement $ ds)
        let stmts = map (\(DefinitionStatement d) -> d) stmts'
        LetExpression (filter is_inline stmts) <$> optimize ex

        where

            is_inline (Definition _ True _ _) = False
            is_inline _ = True

    optimize (RecordExpression (M.toList -> xs)) =

        let (keys, vals) = unzip xs
        in  RecordExpression . M.fromList . zip keys <$> mapM optimize vals

    optimize (JSExpression x) = return $ JSExpression x
    optimize (LiteralExpression x) = return $ LiteralExpression x


-- TODO wrong
instance Optimize (Match (Expression Definition)) where

    optimize (Match ms (Just ex)) = Match ms . Just <$> optimize ex
    optimize x = return x

instance Optimize (Axiom (Expression Definition)) where

    optimize t @ (TypeAxiom _) = return t
    optimize (EqualityAxiom m ex) =

        do m' <- optimize m
           ex' <- optimize ex
           return (EqualityAxiom m' ex')

instance Optimize Definition where

    optimize (Definition a True name [eq @ (EqualityAxiom m ex)]) =

        do eq <- optimize eq
           is  <- get_inline
           e   <- get_env
           ns  <- get_namespace
           set_inline (((ns, (InlineSymbol name)), (m, get_addr ex)) : is)
           set_env    ((InlineSymbol name, (m, get_addr ex)) : e)
           return (Definition a True name [eq])

    optimize (Definition a True c (TypeAxiom _ : x)) = optimize (Definition a True c x)
    optimize (Definition _ True name _) = fail$ "Illegal inline definition '" ++ show name ++ "'"

    optimize (Definition a b name xs) | is_recursive name xs =

       do xs' <- mapM optimize xs
          add_tco $ show name
          return $ Definition a b name (tail_call_optimize name xs')

    optimize (Definition a b c xs) = Definition a b c <$> mapM optimize xs

instance Optimize Statement where

    optimize (DefinitionStatement d) = DefinitionStatement <$> optimize d
    optimize (ExpressionStatement (Addr s e x)) = ExpressionStatement . Addr s e <$> optimize x
    optimize (ModuleStatement x xs) = do

        ns <- get_namespace
        set_namespace$ ns `mappend` x
        xs' <- with_env$ optimize xs
        set_namespace ns
        return$ ModuleStatement x xs'

        where

            get_defs [] = []
            get_defs (DefinitionStatement d : xs) = [d] : get_defs xs
            get_defs (_ : xs) = get_defs xs

    optimize ss @ (ImportStatement (Namespace x) (Just alias)) = 

        do is <- get_inline
           e  <- get_env
           n  <- get_namespace
           rfind n is e

        where rfind (Namespace n) is e =

                     case lookup' (Namespace x) is of

                       [] ->

                           if length n > 0 && head n /= head x
                           then do optimize (ImportStatement (Namespace (head n : x)) Nothing)
                                   return ss
                           else return ss

                       zs ->

                           do set_env $ cc zs ++ e
                              return ss

              cc (((_, s), ex): zs) = (s, ex) : cc zs
              cc [] = []

              lookup' x (((y, (InlineSymbol z)), w):ys)
                  | x == y    = (((y, (InlineRecord (AccessorExpression (Addr  (newPos "Optimizer" 0 0) (newPos "Optimizer" 0 0) (SymbolExpression (Symbol alias))) [z]))), w) : lookup' x ys)
                  | otherwise = lookup' x ys
              lookup' _ [] = []
         
        
    optimize ss @ (ImportStatement (Namespace x) Nothing) =

        do is <- get_inline
           e  <- get_env
           n  <- get_namespace
           rfind n is e

        where rfind (Namespace n) is e =

                     case lookup' (Namespace x) is of

                       [] ->

                           if length n > 0 && head n /= head x
                           then do optimize (ImportStatement (Namespace (head n : x)) Nothing)
                                   return ss
                           else return ss

                       zs ->

                           do set_env $ cc zs ++ e
                              return ss

              cc (((_, s), ex): zs) = (s, ex) : cc zs
              cc [] = []

              lookup' x (((y, z), w):ys)
                  | x == y    = (((y, z), w) : lookup' x ys)
                  | otherwise = lookup' x ys
              lookup' _ [] = []

    optimize x = return x

instance Optimize [Statement] where

    optimize xs = do
        let (tests, defs) = L.partition is_expression xs
        xs <- mapM optimize (sorted_defs defs)
        ys <- mapM optimize tests
        return (xs ++ ys)

        where
            is_expression (ExpressionStatement _) = True
            is_expression _ = False

instance Optimize Program where

    optimize (Program xs) = Program <$> optimize xs

gen_state as = OptimizeState (Namespace []) as [] [] []

run_optimizer :: Program -> OptimizeState -> (OptimizeState, Program)
run_optimizer p @ (optimize -> Optimizer f) as = f as

          

