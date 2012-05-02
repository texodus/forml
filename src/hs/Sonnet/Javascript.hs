{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Sonnet.Javascript (render, render_spec) where

import Language.Javascript.JMacro

import Data.Monoid

import Sonnet.Parser.AST

render :: Program -> String
render (Program xs) = show . renderJs . toStat . map (Qual (Namespace []) (build_modules xs)) $ xs

-- Tests

data Spec = Spec Namespace [Module] Statement

data Qual a = Qual Namespace [Module] a

data Module = Module Namespace [Module]
            | Var String

instance Show Module where
    show (Module (Namespace (reverse -> (x:xs))) _) = x
    show (Var s) = s

render_spec :: Program -> String
render_spec (Program xs) = show . renderJs . toStat . map (Spec (Namespace []) (build_modules xs)) $ xs

build_modules :: [Statement] -> [Module]
build_modules (ModuleStatement n ns : xs) = Module n (build_modules ns) : build_modules xs
build_modules (DefinitionStatement (Definition n _): xs) = Var n : build_modules xs
build_modules (_ : xs) = build_modules xs
build_modules [] = []

declare_this :: String -> JExpr -> JStat
declare_this name expr = BlockStat [ DeclStat (StrI name) Nothing
                                   , AssignStat (ref name) expr
                                   , AssignStat (IdxExpr (ref "this") (ValExpr (JStr name))) (ref name) ]


declare :: String -> JExpr -> JStat
declare name expr = BlockStat [ DeclStat (StrI name) Nothing
                              , AssignStat (ref name) expr ]

ref :: String -> JExpr 
ref name = (ValExpr (JVar (StrI name)))

func :: String -> JStat -> JStat
func var ex = ReturnStat (ValExpr (JFunc [StrI var] (BlockStat [ex])))
              
open :: [String] -> [Statement] -> JStat
open _ [] = mempty
open ns (DefinitionStatement (Definition n _):xs) =

    let x = [jmacroE| `(ref $ show $ Namespace (reverse ns))`[`(n)`] |] in

    [jmacro| `(declare n x)`;
             `(open ns xs)`; |]

open nss (ModuleStatement (Namespace ns @ (head . reverse -> n)) _:xs) =

    [jmacro| `(declare n (ref . show . Namespace $ nss ++ ns))`;
             `(open nss xs)`; |]

open ns (_:xs) = [jmacro| `(open ns xs)`; |]


instance ToStat Spec where
    toStat (Spec (Namespace n) ms (ModuleStatement (Namespace ns) xs))  = 

        [jmacro| describe(`(show $ Namespace ns)`, function() {
                     `(open (ns ++ n) xs)`;
                     var x = new (function {
                         `(map (Spec (Namespace $ ns ++ n) ms) xs)`;
                     }());
                 }); |]

    toStat (Spec n ms i @ (ImportStatement _))  = toStat $ Qual n ms i

    toStat (Spec _ _ (ExpressionStatement e)) =

        [jmacro| it(`(show e)`, function() {
                     expect(function() {
                         var x = new (function() {
                             this.__result__ = `(e)`;
                         });
                         return x.__result__;
                     }()).toEqual(true);
                 }); |]

    toStat _ = mempty

instance (ToStat a) => ToStat [a] where
    toStat = foldl1 mappend . map toStat

instance ToStat (Qual Statement) where
    toStat (Qual _ _ (TypeStatement d x))     = mempty
    toStat (Qual _ _ (ExpressionStatement e)) = mempty
    toStat (Qual name m (ImportStatement d))  =

        let find (Var s : ss) n @ (Namespace []) = s : find ss n
            find (Module _ xs : ms) n @ (Namespace []) = map show xs ++ find ms n
            find [] (Namespace []) = []
            find [] n = find m (name `mappend` n) --error $ "\nUnknown namespace " ++ show n
            find (Var _: xs) ns = find xs ns
            find (Module (Namespace ys) x : zs) n @ (Namespace xs)
                 | length xs >= length ys && take (length ys) xs == ys  = find x (Namespace $ drop (length ys) xs)
                 | otherwise = find zs n
            find a b = error $ "\n" ++ show a ++ "\n" ++ show b
            
            print' []     = error "Empty Namespace"
            print' (x:[]) = [jmacroE| `(ref x)` |]
            print' (x:xs) = [jmacroE| `(print' xs)`[`(x)`] |]

            open' [] _ = mempty
            open' (x:xs) (Namespace ns) =

                declare x [jmacroE| `(print' $ reverse ns)`[`(x)`] |] `mappend` open' xs (Namespace ns)

        in open' (find m d) d

    toStat (Qual (Namespace pns) m (ModuleStatement ns @ (Namespace nnn) xs)) = 
        
        declare_this (show ns) [jmacroE| new (function() { `(fmap (Qual (Namespace (pns ++ nnn)) m) xs)`; }) |]

        -- [jmacro| `(ns)` = new (function() { 
        --              `(fmap (Qual m) xs)`; 
        --          })(); |]

    toStat (Qual _ _ (DefinitionStatement d)) = toStat d

instance ToStat Definition where
    toStat (Definition name as) = declare_this name $ toJExpr as

instance ToJExpr Namespace where
    toJExpr (Namespace xs) = f (reverse xs)
        where f (y:ys)     = [jmacroE| `(f ys)`[`(y)`] |]
              f []         = [jmacroE| this |]

data PatternPair = PP String Pattern

instance ToJExpr [Axiom] where
    toJExpr [] = [jmacroE| null |]
    toJExpr (TypeAxiom _:xs) = toJExpr xs
    toJExpr xs @ (EqualityAxiom (Match ps _) _ : _) = 

        let curry 0 jexpr = jexpr
            curry n jexpr = func (local $ n - 1) (curry (n - 1) jexpr)

            local n = "__" ++ [ "abcdefghijklmnopqrstuvqxyz" !! n ]

            declare_bindings :: [Pattern] -> JStat
            declare_bindings [] = mempty
            declare_bindings (VarPattern x : zs) = declare x [jmacroE| null |] `mappend` declare_bindings zs
            declare_bindings (_ : zs) = declare_bindings zs


            body [] = [jmacro| args = []; console.log "Pattern match exhausted" |]
            body (EqualityAxiom (Match pss cond) ex : xss) = 
                let x :: [PatternPair]
                    x = zipWith PP (reverse . take (length pss) . map local $ [0 .. 26]) pss in
                [jmacro| `(declare_bindings pss)`;
                         if (`(x)` && `(cond)`) {
                             return `(ex)`;
                         } else `(body xss)`; |]

        in  [jmacroE| (function() {
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
            app []     = ref f

        in app (reverse xs)

    toJExpr (SymbolExpression x) = ref x
    toJExpr (NamedExpression x y) = [jmacroE| (function(){ var g = {}; g[`(x)`] = `(y)`; return g})() |]
    toJExpr (IfExpression x y z)  = [jmacroE| (function(){ if (`(x)`) { return `(y)`; } else { return `(z)` }})() |] 
    toJExpr (JSExpression z) = toJExpr False
    toJExpr (FunctionExpression x) = toJExpr x
    toJExpr (RecordExpression m) = toJExpr m
    toJExpr (InheritExpression a b) = toJExpr False
    toJExpr (LetExpression bs ex) = [jmacroE| new function() { `(bs)`; return `(ex)` } |]
    toJExpr _ = toJExpr False

instance ToJExpr [PatternPair] where
    toJExpr [] = toJExpr True
    toJExpr (x:[]) = toJExpr x
    toJExpr (x:xs) = [jmacroE| `(x)` && `(xs)` |]

instance ToJExpr PatternPair where
    toJExpr (PP _ AnyPattern) = toJExpr True
    toJExpr (PP n (VarPattern x)) = [jmacroE| (function() { `(ref x)` = `(ref n)`; return true; })() |]
    toJExpr (PP n (LiteralPattern x)) = [jmacroE| `(ref n)` === `(x)` |]
    toJExpr (PP _ x) = toJExpr False -- error $ show x

instance ToJExpr Literal where
    toJExpr (StringLiteral s) = toJExpr s
    toJExpr (IntLiteral s)    = toJExpr s
    toJExpr (DoubleLiteral s) = toJExpr s


