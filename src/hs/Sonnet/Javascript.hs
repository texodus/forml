{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Sonnet.Javascript (render, render_spec) where

import Control.Applicative

import Language.Javascript.JMacro

import Data.Char
import Data.List
import Data.Monoid
import Data.Functor.Identity
import Control.Monad.State.Strict as D


import qualified Data.Map as M
import qualified Data.Set as S

import Sonnet.Parser.AST

render :: Program -> String
render (Program xs) = show . renderJs . toStat $ xs

-- Tests

data Spec = Spec Namespace Statement

render_spec :: Program -> String
render_spec (Program xs) = show . renderJs . toStat . map (Spec $ Namespace []) $ xs

declare_this :: String -> JExpr -> JStat
declare_this name expr = BlockStat [ DeclStat (StrI name) Nothing
                                   , AssignStat (ref name) expr
                                   , AssignStat (IdxExpr (ref "this") (ValExpr (JStr name))) (ref name) ]


declare :: String -> JExpr -> JStat
declare name expr = BlockStat [ DeclStat (StrI name) Nothing
                              , AssignStat (ref name) expr ]

ref :: String -> JExpr 
ref name = (ValExpr (JVar (StrI name)))
              
open :: [String] -> JStat
open (reverse -> xs) = [jmacro| for (el in `(ref $ show $ Namespace xs)`) { 
                       console.log el;
                       this[el] = `(ref $ show $ Namespace xs)`[el]; 
                   } |]

instance ToStat Spec where
    toStat (Spec (Namespace n) (ModuleStatement (Namespace ns) xs))  = 

        [jmacro| describe(`(show $ Namespace ns)`, function() {
                     // this["test"] = 1;
                     var x = new (function {
                         `(map (Spec (Namespace $ ns ++ n)) xs)`;
                     }());
                 }); |]

    toStat (Spec (Namespace n) (ExpressionStatement e)) =

        [jmacro| it(`(show e)`, function() {
                     expect(function() {
                         var x = new (function() {
                             `(open n)`;
                             this.__result__ = `(e)`;
                         });
                         return x.__result__;
                     }()).toEqual(true);
                 }); |]

    toStat _ = mempty

instance (ToStat a) => ToStat [a] where
    toStat = foldl1 mappend . map toStat

instance ToStat Statement where
    toStat (TypeStatement d x)     = mempty
    toStat (ExpressionStatement e) = mempty
    toStat (ImportStatement d)     = mempty

    toStat (ModuleStatement ns xs) = 

        [jmacro| `(ns)` = new (function() { 
                     `(xs)`; 
                 })(); |]

    toStat (DefinitionStatement d) = toStat d

instance ToStat Definition where
    toStat (Definition name as) = declare_this name (toJExpr as) -- [jmacroE| this[`(name)`] = `(as)` |]

instance ToJExpr Namespace where
    toJExpr (Namespace xs) = f (reverse xs)
        where f (y:ys)     = [jmacroE| `(f ys)`[`(y)`] |]
              f []         = [jmacroE| this |]

instance ToJExpr [Axiom] where
    toJExpr [] = [jmacroE| null |]
    toJExpr (TypeAxiom _:xs) = toJExpr xs
    toJExpr xs @ (EqualityAxiom (Match ps _) _ : _) = 

        let curry 0 jexpr = jexpr
            curry n jexpr = [jmacro| return function(y) { args.push y; `(curry (n - 1) jexpr)`; } |]

            declare_bindings :: [Pattern] -> JStat
            declare_bindings [] = mempty
            declare_bindings (VarPattern x : zs) = declare x [jmacroE| null |] `mappend` declare_bindings zs
            declare_bindings (_ : zs) = declare_bindings zs


            body [] = [jmacro| args = []; console.log "Pattern match exhausted" |]
            body (EqualityAxiom (Match pss cond) ex : xss) = 

                [jmacro| current = 0;
                         `(declare_bindings pss)`;
                         if (`(pss)` && `(cond)`) {
                             args = [];
                             return `(ex)`;
                         } else `(body xss)`; |]

        in  [jmacroE| (function() {
                          var !args = [];
                          var !current;
                          `(curry (length ps) (body xs))`;
                      })() |]

-- This instance handles pattern guards
instance ToJExpr (Maybe Expression) where
    toJExpr Nothing  = toJExpr True
    toJExpr (Just x) = toJExpr x

instance ToJExpr Expression where
    toJExpr (LiteralExpression l) = toJExpr l
    toJExpr (ApplyExpression (SymbolExpression "+") [x, y]) = [jmacroE| `(x)` + `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression "-") [x, y]) = [jmacroE| `(x)` - `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression "==") [x, y]) = [jmacroE| `(x)` === `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression f) xs) = 

        let app (x:xs) = [jmacroE| `(app xs)`(`(x)`) |]
            app []     = [jmacroE| this[`(f)`] |]

        in app (reverse xs)

    toJExpr (SymbolExpression x) = ref x
    toJExpr (NamedExpression x y) = [jmacroE| (function(){ var g = {}; g[`(x)`] = `(y)`; return g})() |]
    toJExpr (IfExpression x y z)  = [jmacroE| (function(){ if (`(x)`) { return `(y)`; } else { return `(z)` }})() |] 
    toJExpr (JSExpression z) = undefined
    toJExpr (FunctionExpression x) = toJExpr x
    toJExpr (RecordExpression m) = toJExpr m
    toJExpr (InheritExpression a b) = undefined
    toJExpr (LetExpression bs ex) = [jmacroE| new function() { `(bs)`; return `(ex)` } |]

instance ToJExpr [Pattern] where
    toJExpr [] = toJExpr True
    toJExpr (x:[]) = toJExpr x
    toJExpr (x:xs) = [jmacroE| `(x)` && (function() { current++; return `(xs)`; })() |]

instance ToJExpr Pattern where
    toJExpr AnyPattern = toJExpr True
    toJExpr (VarPattern x) = [jmacroE| (function() { `(ref x)` = args[current]; return true; })() |]
    toJExpr (LiteralPattern x) = [jmacroE| args[current] === `(x)` |]
    toJExpr x = error $ show x

instance ToJExpr Literal where
    toJExpr (StringLiteral s) = toJExpr s
    toJExpr (IntLiteral s)    = toJExpr s
    toJExpr (DoubleLiteral s) = toJExpr s


