
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
import System.IO.Unsafe()

import Language.Javascript.JMacro

import qualified Data.Map as M

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Char

import Formal.Types.Pattern
import Formal.Types.Symbol
import Formal.Types.Expression
import Formal.Types.Definition
import Formal.Types.Axiom
import Formal.Types.Statement hiding (find, namespace, modules, Test)
import Formal.Types.Namespace hiding (Module)

import Formal.TypeCheck hiding (get_namespace)

import Formal.Parser.Utils
import Formal.Parser

import qualified Formal.Javascript.Utils as J

import Prelude hiding (curry)




type Inlines = [((Namespace, Symbol), (Match (Expression Definition), Expression Definition))]
type Inline  = [(Symbol, (Match (Expression Definition), Expression Definition))]

data OptimizeState = OptimizeState { ns :: Namespace
                                   , assumptions :: [(Namespace, [Assumption])]
                                   , inlines :: Inlines
                                   , tco :: [String]
                                   , env :: Inline }

data Optimizer a = Optimizer (OptimizeState -> (OptimizeState, a))

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

get_addr :: Addr a -> a
get_addr (Addr _ _ x) = x

instance (Optimize a) => Optimize (Addr a) where

    optimize (Addr s e a) = Addr s e <$> optimize a

instance Optimize (Expression Definition) where

    optimize (ApplyExpression f' @ (SymbolExpression f) args) =

        do is <- get_env
           case f `lookup` is of
             Just (m @ (Match pss _), ex) | length pss == length args ->

                 do args' <- mapM optimize args
                    ex'   <- optimize ex
                    m'    <- optimize m
                    return $ ApplyExpression (FunctionExpression [EqualityAxiom m' (Addr undefined undefined ex')]) args'


             _ -> ApplyExpression <$> optimize f' <*> mapM optimize args
             



    optimize (ApplyExpression f args) = ApplyExpression <$> optimize f <*> mapM optimize args
    optimize (IfExpression a b c) = IfExpression <$> optimize a <*> optimize b <*> optimize c
    optimize (LazyExpression x l) = flip LazyExpression l <$> optimize x
    optimize (FunctionExpression xs) = FunctionExpression <$> mapM optimize xs
    optimize (AccessorExpression x xs) = flip AccessorExpression xs <$> optimize x
    optimize (ListExpression ex) = ListExpression <$> mapM optimize ex

    optimize (LetExpression ds ex) =

        do ds' <- mapM optimize ds
           LetExpression ds' <$> optimize ex

    optimize (RecordExpression (M.toList -> xs)) =

        let (keys, vals) = unzip xs
        in  RecordExpression . M.fromList . zip keys <$> mapM optimize vals

    optimize x = return x


-- TODO wrong
instance Optimize (Match (Expression Definition)) where

    optimize (Match ms (Just ex)) = Match ms . Just <$> optimize ex
    optimize x = return x

instance Optimize (Axiom (Expression Definition)) where

    optimize t @ (TypeAxiom _) = return t
    optimize (EqualityAxiom m ex) =

        do m <- optimize m
           ex <- optimize ex
           return (EqualityAxiom m ex)

instance Optimize Definition where

    optimize (Definition a True name [eq @ (EqualityAxiom _ _)]) =

        do (EqualityAxiom m ex) <- optimize eq
           is  <- get_inline
           e   <- get_env
           ns  <- get_namespace
           set_inline (((ns, name), (m, get_addr ex)) : is)
           set_env    ((name, (m, get_addr ex)) : e)
           return (Definition a True name [EqualityAxiom m ex])

    optimize (Definition a True c (TypeAxiom _ : x)) = optimize (Definition a True c x)
    optimize (Definition _ True name _) = fail$ "Illegal inline definition '" ++ show name ++ "'"

    optimize (Definition a b name xs) | is_recursive xs =

       do xs' <- mapM optimize xs
          add_tco $ show name
          return $ Definition a b name (axioms xs')

       where is_recursive (TypeAxiom _: xs) = is_recursive xs
             is_recursive (EqualityAxiom _ x: xs) = is_recursive' (get_addr x) || is_recursive xs
             is_recursive [] = False

             is_recursive' (ApplyExpression (SymbolExpression x) _) | name == x = True
             is_recursive' (LetExpression _ e) = is_recursive' e
             is_recursive' (IfExpression _ a b) = is_recursive' a || is_recursive' b
             is_recursive' _ = False

             axioms (t @ (TypeAxiom _): xs) = t : axioms xs
             axioms xs' =
                  [EqualityAxiom (Match [] Nothing) (Addr undefined undefined (JSExpression (to_trampoline xs')))]

             to_trampoline xs @ (EqualityAxiom (Match ps _) _ : _) =
                 J.scope . J.curry (length ps) ("_V"++) . to_trampoline' ps $ xs

             to_trampoline' ps xs =

                 [jmacro| var __result = undefined;
                          `(def_local (reverse . take (length ps) . map J.local_pool $ [0 .. 26]) local_var_names)`;
                          while (typeof __result == "undefined") {
                              (function() {
                                 `(to_trampoline'' xs __result)`;
                              })();
                          }
                          return __result; |]

                 where to_trampoline'' [] _ = [jmacro| exhaust(); |]
                       to_trampoline'' (EqualityAxiom (Match pss cond) (Addr _ _ ex) : xss) result =

                           [jmacro| `(declare_bindings var_names pss)`;
                                    if (`(pss)` && `(cond)`) {
                                        `(result)` = `(replace pss ex)`;
                                    } else `(to_trampoline'' xss result)`; |]

                       var_names = map J.ref . reverse . take (length ps) . map J.local_pool $ [0 .. 26]

                       --to_expr ps = toJExpr$ zipWith PM (reverse . take (length ps) . map ("r"++) . map J.local_pool $ [0 .. 26]) ps

                       local_var_names = map J.ref . map ("_V"++) . reverse . take (length ps) . map J.local_pool $ [0 .. 26]

                       declare_bindings (name : names) (VarPattern x : zs) =
                           
                           [jmacro| `(J.declare x name)`; |] `mappend` declare_bindings names zs

                       declare_bindings (name : names) (RecordPattern x _: zs) = 
                           let (ns, z) = unzip . M.toList $ x
                           in  declare_bindings (map (acc name) ns) z `mappend` declare_bindings names zs

                       declare_bindings (_ : names) (_ : zs) = declare_bindings names zs
                       declare_bindings _ _ = mempty
                  
                       acc n ns = [jmacroE| `(n)`[`(ns)`] |]

                       replace pss (ApplyExpression (SymbolExpression x) args) | name == x =
             
                           JSExpression [jmacroE| (function() {
                                                     `(bind_local (reverse . take (length ps) . map J.local_pool $ [0 .. 26]) args)`;
                                                     return undefined;
                                                  })() |]

                       replace pss (LetExpression x e) = LetExpression x (replace pss e)
                       replace pss (IfExpression x a b) = IfExpression x (replace pss a) (replace pss b)
                       replace _ x = x

                       bind_local :: ToJExpr a => [String] -> [a] -> JStat
                       bind_local (x:xs) (y:ys) = [jmacro|  `(J.ref x)` = `(y)`; |] `mappend` bind_local xs ys
                       bind_local _ _ = mempty

                       def_local :: [String] -> [JExpr] -> JStat
                       def_local (x:xs) (y:ys) = [jmacro| `(J.declare x y)`; |] `mappend` def_local xs ys
                       def_local _ _ = mempty




    optimize (Definition a b c xs) = Definition a b c <$> mapM optimize xs

instance Optimize Statement where

    optimize (DefinitionStatement d) = DefinitionStatement <$> optimize d
    optimize (ExpressionStatement (Addr s e x)) = ExpressionStatement . Addr s e <$> optimize x
    optimize (ModuleStatement x xs) =

        do ns <- get_namespace
           set_namespace$ ns `mappend` x
           xs' <- with_env$ optimize xs
           set_namespace ns
           return$ ModuleStatement x xs'

    optimize ss @ (ImportStatement (Namespace x)) =

        do is <- get_inline
           e  <- get_env
           n  <- get_namespace
           rfind n is e

        where rfind (Namespace n) is e =

                     case lookup' (Namespace x) is of
                       [] ->

                           if length n > 0 && head n /= head x
                           then do optimize (ImportStatement (Namespace (head n : x)))
                                   return ss
                           else return ss

                       zs ->

                           do set_env $ cc zs ++ e 
                              return ss

              cc (((_, s), ex): zs) = (s, ex) : cc zs
              cc [] = []

              lookup' x (((y, z), w):ys) | x == y = (((y, z), w) : lookup' x ys)
                                         | otherwise = lookup' x ys
              lookup' _ [] = []
                                         
    optimize x = return x

instance Optimize [Statement] where

    optimize [] = return []
    optimize (x:xs) =

        do x <- optimize x
           xs <- optimize xs
           return (x:xs)

instance Optimize Program where

    optimize (Program xs) = Program <$> optimize xs

run_optimizer :: Program -> [(Namespace, [Assumption])] -> Program
run_optimizer (optimize -> Optimizer f) as = case f gen_state of ((tco -> s), p) -> p --error $ show s

    where gen_state = OptimizeState (Namespace []) as [] [] []
                       