{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Sonnet.Javascript (render, render_spec) where

import Text.InterpolatedString.Perl6

import Language.Javascript.JMacro

import Data.Monoid

import Sonnet.Parser.AST
import Prelude hiding (curry)

render :: Program -> String
render (Program xs) = show . renderJs . toStat . map (Qual (Namespace []) (build_modules xs)) $ xs

-- Tests

data Spec = Spec Namespace [Module] Statement

data Qual a = Qual Namespace [Module] a

data Module = Module Namespace [Module]
            | Var String

instance Show Module where
    show (Module (Namespace (reverse -> (x:_))) _) = x
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
open ns (DefinitionStatement (Definition n _) : xs) =

    let f = ref . show . Namespace
        x = [jmacroE| `(f ns)`[`(n)`] |] in

    [jmacro| `(declare n x)`;
             `(open ns xs)`; |]

open nss (ModuleStatement (Namespace ns @ (n:_)) _:xs) =

    [jmacro| `(declare n (ref . show $ Namespace nss `mappend` Namespace ns))`;
             `(open nss xs)`; |]

open ns (_ : xs) = open ns xs


instance ToStat Spec where
    toStat (Spec (Namespace n) ms (ModuleStatement (Namespace ns) xs))  = 

        [jmacro| describe(`(show $ Namespace ns)`, function() {
                     `(open (n ++ ns) xs)`;
                     var x = new (function {
                         `(map (Spec (Namespace $ n ++ ns) ms) xs)`;
                     }());
                 }); |]

    toStat (Spec n ms i @ (ImportStatement _))  = toStat $ Qual n ms i

    toStat (Spec _ _ (ExpressionStatement e)) =

        [jmacro| it(`(show e)`, function() {
                     expect((function() {
                         try {
                             return `(e)`;
                         } catch (ex) {
                             return { error: ex };
                         }
                     })()).toEqual(true);
                 }); |]

    toStat _ = mempty

instance (ToStat a) => ToStat [a] where
    toStat = foldl1 mappend . map toStat

-- instance Monoid (Maybe [String]) where
--     mempty = Nothing
--     mappend = undefined


instance ToStat (Qual Statement) where
    toStat (Qual _ _ (TypeStatement _ _))     = mempty
    toStat (Qual _ _ (ExpressionStatement _)) = mempty
    toStat (Qual current_ns mm (ImportStatement target_ns))  =

        let find :: Namespace -> ([String], Namespace)
            find (Namespace []) =

                case find' mm target_ns of
                  Just y -> (y, target_ns)
                  Nothing -> error $ [qq| Could not resolve namespace $target_ns from namespace $current_ns |]

            find current @ (Namespace current'') =

                case find' mm (current `mappend` target_ns) of
                  Just y -> (y, (Namespace (drop (length current'') current'')) `mappend` target_ns)
                  Nothing -> find (Namespace $ take (length current'' - 1) current'') 
                

            find' :: [Module] -> Namespace -> Maybe [String]
            find' (Var s : ss) n @ (Namespace []) = Just [s] `mappend` find' ss n
            find' (Module _ xs : ms) n @ (Namespace []) = Just (map show xs) `mappend` find' ms n
            find' [] (Namespace []) = Just []
            find' [] _ = Nothing
            find' (Var _: xs) ns = find' xs ns
            find' (Module (Namespace ys) x : zs) n @ (Namespace xs)
                 | length xs >= length ys && take (length ys) xs == ys = 
                     find' x (Namespace $ drop (length ys) xs)
                 | otherwise = find' zs n
-- --            find' a b = error $ [qq| Could not resolve namespace $b from namespace $a |]
            
            print' []     = error "Empty Namespace"
            print' (x:[]) = [jmacroE| `(ref x)` |]
            print' (x:xs) = [jmacroE| `(print' xs)`[`(x)`] |]

            open' ([], _) = mempty
            open' (x:xs, Namespace ns) =

                declare x [jmacroE| `(print' $ reverse ns)`[`(x)`] |] `mappend` open' (xs, (Namespace ns))

        in open' (find current_ns) 

    toStat (Qual pns m (ModuleStatement ns xs)) = 
        
        declare_this (show ns) [jmacroE| new (function() { 
                                             `(fmap (Qual (pns `mappend` ns) m) xs)`;
                                         }) |]

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

            local n = [qq|__{ "abcdefghijklmnopqrstuvqxyz" !! n }__|]

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

        let app (y:ys) = [jmacroE| `(app ys)`(`(y)`) |]
            app []     = ref f

        in app (reverse xs)

    toJExpr (SymbolExpression x)    = ref x
    toJExpr (NamedExpression x y)   = [jmacroE| (function(){ var g = {}; g[`(x)`] = `(y)`; return g})() |]
    toJExpr (IfExpression x y z)    = [jmacroE| (function(){ if (`(x)`) { return `(y)`; } else { return `(z)` }})() |] 
    toJExpr (JSExpression _)        = toJExpr False
    toJExpr (FunctionExpression x)  = toJExpr x
    toJExpr (RecordExpression m)    = toJExpr m
    toJExpr (InheritExpression _ _) = toJExpr False
    toJExpr (LetExpression bs ex)   = [jmacroE| new function() { `(bs)`; return `(ex)` } |]
    toJExpr _                       = toJExpr False

instance ToJExpr [PatternPair] where
    toJExpr [] = toJExpr True
    toJExpr (x:[]) = toJExpr x
    toJExpr (x:xs) = [jmacroE| `(x)` && `(xs)` |]

instance ToJExpr PatternPair where
    toJExpr (PP _ AnyPattern)         = toJExpr True
    toJExpr (PP n (VarPattern x))     = [jmacroE| (function() { `(ref x)` = `(ref n)`; return true; })() |]
    toJExpr (PP n (LiteralPattern x)) = [jmacroE| `(ref n)` === `(x)` |]
    toJExpr (PP _ x)                  = error $ "Unimplemented " ++ show x

instance ToJExpr Literal where
    toJExpr (StringLiteral s) = toJExpr s
    toJExpr (IntLiteral s)    = toJExpr s
    toJExpr (DoubleLiteral s) = toJExpr s


