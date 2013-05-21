
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

module Forml.Optimize.TailCall where
import System.IO.Unsafe ()

import Control.Monad

import qualified Data.Map as M

import Data.Monoid

import Language.Javascript.JMacro

import Forml.Types.Symbol
import Forml.Types.Axiom
import Forml.Types.Expression
import Forml.Types.Definition
import Forml.Types.Pattern
import Forml.Parser.Utils
import qualified Forml.Javascript.Utils as J

import Prelude hiding (curry)

get_addr :: Addr a -> a
get_addr (Addr _ _ x) = x

is_recursive name (TypeAxiom _: xs') = is_recursive name xs'
is_recursive name (EqualityAxiom _ x: xs') = is_recursive' name (get_addr x) || is_recursive name xs'
is_recursive name [] = False

is_recursive' name (ApplyExpression (SymbolExpression x) _) | name == x = True
is_recursive' name (LetExpression _ e) = is_recursive' name e
is_recursive' name (IfExpression _ a (Just b)) = is_recursive' name a || is_recursive' name b
is_recursive' name (IfExpression _ a _) = is_recursive' name a
is_recursive' name _ = False


tail_call_optimize ::
    Symbol -> [Axiom (Expression Definition)] -> [Axiom (Expression Definition)]

tail_call_optimize name (t @ (TypeAxiom _): xs) =
    t : tail_call_optimize name xs

tail_call_optimize name xs' =
    [EqualityAxiom (Match [] Nothing) (Addr undefined undefined (JSExpression (to_trampoline xs')))]

    where
        to_trampoline xs @ (EqualityAxiom (Match ps _) _ : _) =
            opt . J.scope . J.curry (length ps) ("_V"++) . to_trampoline' ps $ xs

        to_trampoline' ps xs =

            [jmacro| var __result = undefined;
                     `(def_local (reverse . take (length ps) . map J.local_pool $ [0 .. 26]) local_var_names)`;
                     while (typeof __result == "undefined") {
                            `(to_trampoline'' xs __result)`;
                         
                     }
                     return __result; |]

            where to_trampoline'' [] _ = [jmacro| exhaust(); |]
                  to_trampoline'' (EqualityAxiom (Match pss cond) (Addr _ _ ex) : xss) result =

                      case [jmacroE| `(pss)` && `(cond)` |] of
                          InfixExpr "&&" (ValExpr (JVar (StrI "true"))) (ValExpr (JVar (StrI "true"))) -> [jmacro|
                              `(declare_bindings var_names pss)`;
                              `(result)` = `(replace pss ex)`;
                          |]

                          x -> [jmacro|
                              `(declare_bindings var_names pss)`;
                              if (`(x)`) {
                                  `(result)` = `(replace pss ex)`;
                              } else `(to_trampoline'' xss result)`;
                          |]

                  var_names = map J.ref . reverse . take (length ps) . map J.local_pool $ [0 .. 26]

                  local_var_names = map J.ref . map ("_V"++) . reverse . take (length ps) . map J.local_pool $ [0 .. 26]

                  declare_bindings (name : names) (VarPattern x : zs) =

                      [jmacro| `(J.declare x name)`; |] `mappend` declare_bindings names zs

                  declare_bindings (name : names) (RecordPattern x _: zs) =
                      let (ns, z) = unzip . M.toList $ x
                      in  declare_bindings (map (acc name) ns) z `mappend` declare_bindings names zs

                  declare_bindings (_ : names) (_ : zs) = declare_bindings names zs
                  declare_bindings _ _ = mempty

                  acc n ns = [jmacroE| `(n)`[`(ns)`] |]

                  replace _ (ApplyExpression (SymbolExpression x) args) | name == x =

                      JSExpression [jmacroE| (function() {
                                                `(bind_local (reverse . take (length ps) . map J.local_pool $ [0 .. 26]) args)`;
                                                return undefined;
                                             })() |]

                  replace pss (LetExpression x e) = LetExpression x (replace pss e)
                  replace pss (IfExpression x a b) = IfExpression x (replace pss a) (replace pss `fmap` b)
                  replace _ x = x

                  bind_local :: ToJExpr a => [String] -> [a] -> JStat
                  bind_local (x:xs) (y:ys) = [jmacro|  `(J.ref x)` = `(y)`; |] `mappend` bind_local xs ys
                  bind_local _ _ = mempty

                  def_local :: [String] -> [JExpr] -> JStat
                  def_local (x:xs) (y:ys) = [jmacro| `(J.declare x y)`; |] `mappend` def_local xs ys
                  def_local _ _ = mempty
