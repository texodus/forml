
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

import Data.Monoid

import Forml.Types.Axiom
import Forml.Types.Definition
import Forml.Types.Expression
import Forml.Types.Namespace  hiding (Module)
import Forml.Types.Pattern
import Forml.Types.Statement  hiding (Test, find, modules, namespace)
import Forml.Types.Symbol
import Forml.Deps

import Forml.Parser
import Forml.Parser.Utils

import Forml.Optimize.TailCall
import Forml.Optimize.Inline
import Forml.Optimize.Optimizer

import Prelude hiding (curry)
import Text.Parsec.Pos (newPos)

instance Optimize (Expression Definition) where

    optimize (ApplyExpression (ApplyExpression a b) c) =
        optimize (ApplyExpression a (b ++ c))

    optimize (ApplyExpression (SymbolExpression s) args) = do
        is <- get_env
        case (InlineSymbol s) `lookup` is of
            Just ((Match pss z), ex)
                | length pss == length args -> do
                    args' <- mapM optimize args
                    optimize $ inline_apply pss args args' ex
                | length pss > length args -> do
                    args' <- mapM optimize args
                    optimize (FunctionExpression 
                        [ EqualityAxiom
                            (Match (drop (length args) pss) z)  
                            (Addr undefined undefined (inline_apply (take (length args) pss) args args' ex)) ])

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

    optimize ss @ (ImportStatement (Namespace x) (Just alias)) = do
        is <- get_inline
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
