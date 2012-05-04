{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Sonnet.Javascript (render, render_spec) where

import Text.InterpolatedString.Perl6
import Language.Javascript.JMacro
import Data.Monoid
import qualified Data.Map as M
import Sonnet.Parser.AST
import Prelude hiding (curry, (++))

prelude :: JStat
prelude = [jmacro| function !equals(x, y) { 
                       if (typeof x == "object" && typeof y == "object") {
                           var result = true;
                           for (key in x) {
                               result = result && equals(x[key], y[key]);
                           };
                           return result;
                       } else { return x === y; }
                   };

                   equals(0, 0); |]

render :: Program -> String
render (Program xs) = show . renderJs . (prelude ++) . toStat . map (empty_meta Library xs) $ xs

render_spec :: Program -> String
render_spec (Program xs) = show . renderJs . toStat . map (empty_meta Test xs) $ xs

--------------------------------------------------------------------------------
----
---- Metadata

data Meta = Meta { target    :: Target,
                   namespace :: Namespace,
                   modules   :: [Module],
                   expr      :: Statement } deriving (Show)

data Target = Test | Library
            deriving (Show)

data Module = Module Namespace [Module]
            | Var String

data PatternMatch = PM String Pattern

newtype Curried = Curried [Axiom]

instance Show Module where
    show (Module (Namespace (reverse -> (x:_))) _) = x
    show (Var s) = s

empty_meta :: Target -> [Statement] -> Statement -> Meta
empty_meta x = Meta x (Namespace []) . build_modules

    where build_modules (ModuleStatement n ns : xs) = Module n (build_modules ns) : build_modules xs
          build_modules (DefinitionStatement (Definition n _): xs) = Var n : build_modules xs
          build_modules (_ : xs) = build_modules xs
          build_modules [] = []

--------------------------------------------------------------------------------
----
---- Utilities

declare          :: String -> JExpr -> JStat
declare_this     :: String -> JExpr -> JStat
ref              :: String -> JExpr 
func             :: String -> JStat -> JStat
find             :: [Module] -> Namespace -> Maybe [String]
end              :: [a] -> [a]
curry            :: Int -> JStat -> JStat
local_pool       :: Int -> String
declare_bindings :: [Pattern] -> JStat
scope            :: ToStat a => a -> JExpr

(++) :: Monoid a => a -> a -> a
(++) = mappend

end (reverse -> x : xs) = x : reverse xs

ref name    = ValExpr (JVar (StrI name))
func var ex = ReturnStat (ValExpr (JFunc [StrI var] (BlockStat [ex])))

declare_this name expr =

    [jmacro| `(declare name expr)`;
             this[`(name)`] = `(ref name)`; |]

declare name expr =

    [jmacro| `(DeclStat (StrI name) Nothing)`;
             `(ref name)` = `(expr)`; |]

find (Var s : ss) n @ (Namespace []) = Just [s] ++ find ss n
find (Var _: xs) ns                  = find xs ns
find [] (Namespace [])               = Just []
find [] _                            = Nothing

find (Module _ xs : ms) n @ (Namespace []) = Just (map show xs) ++ find ms n
find (Module (Namespace ys) x : zs) n @ (Namespace xs)
     | length xs >= length ys && take (length ys) xs == ys = 
         find x (Namespace $ drop (length ys) xs)
     | otherwise = find zs n

curry 0 jexpr = jexpr
curry n jexpr = func (local_pool $ n - 1) (curry (n - 1) jexpr)

local_pool n = [qq|__{ "abcdefghijklmnopqrstuvqxyz" !! n }__|]

declare_bindings [] = mempty
declare_bindings (VarPattern x : zs) = declare x [jmacroE| null |] ++ declare_bindings zs
declare_bindings (RecordPattern x : zs) = 
    let (_, z) = unzip . M.toList $ x
    in  declare_bindings z ++ declare_bindings zs

declare_bindings (_ : zs) = declare_bindings zs

scope x = [jmacroE| (function() { `(x)`; })() |]

class Open a where open :: Namespace -> [a] -> JStat

instance Open Statement where
    open _ [] = mempty
    open ns (DefinitionStatement (Definition n _) : xs) =

        let f = ref . show
            x = [jmacroE| `(f ns)`[`(n)`] |] in

        [jmacro| `(declare n x)`;
                 `(open ns xs)`; |]

    open nss (ModuleStatement ns @ (Namespace (n:_)) _:xs) =

        [jmacro| `(declare n (ref . show $ nss ++ ns))`;
                 `(open nss xs)`; |]

    open ns (_ : xs) = open ns xs

instance Open String where
    open _ [] = mempty
    open (Namespace ns) (x:xs) =

        let print' []     = error "Empty Namespace"
            print' (y:[]) = [jmacroE| `(ref y)` |]
            print' (y:ys) = [jmacroE| `(print' ys)`[`(y)`] |]

        in  declare x [jmacroE| `(print' $ reverse ns)`[`(x)`] |] ++ open (Namespace ns) xs



--------------------------------------------------------------------------------
----
---- JMacro Instances

instance ToStat Meta where

    -- Expressions are ignored for Libraries, and rendered as tests for Test
    toStat (Meta { target = Library, expr = ExpressionStatement _ }) = mempty
    toStat (Meta { target = Test,    expr = ExpressionStatement e }) =

        [jmacro| it(`(show e)`, function() {
                     expect((function() {
                         try {
                             return `(e)`;
                         } catch (ex) {
                             return { error: ex };
                         }
                     })()).toEqual(true);
                 }); |]

    -- Imports work identically for both targets
    toStat (Meta { modules, 
                   namespace = Namespace [], 
                   expr      = ImportStatement target_namespace @ (find modules -> Nothing) }) = 

        error $ [qq| Could not resolve namespace $target_namespace |]  

    toStat (Meta { modules, 
                   expr      = ImportStatement target_namespace, 
                   namespace = (find modules . (++ target_namespace) -> Just y) }) =

        open target_namespace y

    toStat meta @ (Meta { expr = ImportStatement _, .. }) = 

        let slice (Namespace ns) = Namespace . take (length ns - 1) $ ns
        in  toStat (meta { namespace = slice namespace })

    -- Modules in test mode must open the contents of the Library
    toStat meta @ (Meta { target = Library, expr = ModuleStatement ns xs, .. }) =
        
        let ex = [jmacroE| new (function() { 
                               `(fmap (\z -> meta { namespace = namespace ++ ns, expr = z }) xs)`;
                           }) |]

        in declare_this (show ns) ex

    toStat meta @ (Meta { target = Test, expr = ModuleStatement ns xs, .. }) =

        [jmacro| describe(`(show ns)`, function() {
                     `(open (namespace ++ ns) xs)`;
                     var x = new (function {
                         `(map (\z -> meta { namespace = namespace ++ ns, expr = z }) xs)`; 
                     }());
                 }); |]

    -- Definitions are ignored for the Test target
    toStat (Meta { target = Library, expr = DefinitionStatement d }) = toStat d
    toStat (Meta { target = Test,    expr = DefinitionStatement _ }) = mempty

    toStat x = error $ "Unimplemented " ++ show x

instance (ToStat a) => ToStat [a] where
    toStat = foldl1 mappend . map toStat

instance ToStat Definition where
    toStat (Definition name as) = declare_this name $ toJExpr as

instance ToJExpr Namespace where
    toJExpr (Namespace []) = ref "this"
    toJExpr (Namespace (end -> x : xs)) = [jmacroE| `(Namespace xs)`[`(x)`] |]

instance ToJExpr [Axiom] where
    toJExpr [] = toJExpr . scope . Curried $ []
    toJExpr (TypeAxiom _:xs) = toJExpr xs
    toJExpr xs @ (EqualityAxiom (Match ps _) _ : _) = scope . curry (length ps) . toStat . Curried $ xs

instance ToStat Curried where
    toStat (Curried []) = [jmacro| args = []; console.log "Pattern match exhausted" |]
    toStat (Curried (EqualityAxiom (Match pss cond) ex : xss)) = 

        [jmacro| `(declare_bindings pss)`;
                 if (`(pss)` && `(cond)`) {
                     return `(ex)`;
                 } else `(Curried xss)`; |]

instance ToJExpr [Pattern] where
    toJExpr ps = toJExpr $ zipWith PM (reverse . take (length ps) . map local_pool $ [0 .. 26]) ps

instance ToJExpr (Maybe Expression) where
    toJExpr = maybe (toJExpr True) toJExpr

instance ToJExpr Expression where
    toJExpr (ApplyExpression (SymbolExpression "+") [x, y])  = [jmacroE| `(x)` + `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression "-") [x, y])  = [jmacroE| `(x)` - `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression "==") [x, y]) = [jmacroE| equals(`(x)`, `(y)`) |]
    toJExpr (ApplyExpression (SymbolExpression f) [])        = ref f
    toJExpr (ApplyExpression f (end -> x : xs))              = [jmacroE| `(ApplyExpression f xs)`(`(x)`) |]

    toJExpr (LiteralExpression l)   = toJExpr l
    toJExpr (SymbolExpression x)    = ref x
    toJExpr (NamedExpression x y)   = [jmacroE| (function(){ var g = {}; g[`(x)`] = `(y)`; return g})() |]
    toJExpr (IfExpression x y z)    = [jmacroE| (function(){ if (`(x)`) { return `(y)`; } else { return `(z)` }})() |] 
    toJExpr (FunctionExpression x)  = toJExpr x
    toJExpr (RecordExpression m)    = toJExpr m
    toJExpr (LetExpression bs ex)   = [jmacroE| new function() { `(bs)`; return `(ex)` } |]
    toJExpr x                       = error $ "Unimplemented " ++ show x

instance ToJExpr [PatternMatch] where
    toJExpr []     = toJExpr True
    toJExpr (x:xs) = [jmacroE| `(x)` && `(xs)` |]

instance ToJExpr PatternMatch where
    toJExpr (PM _ AnyPattern)         = toJExpr True
    toJExpr (PM n (VarPattern x))     = [jmacroE| (function() { `(ref x)` = `(ref n)`; return true; })() |]
    toJExpr (PM n (LiteralPattern x)) = [jmacroE| `(ref n)` === `(x)` |]
    toJExpr (PM n (RecordPattern (M.toList -> xs))) = toJExpr (map (\(key, val) -> toJExpr (PM (n ++ "." ++ key) val)) xs)
    toJExpr (PM n (RecordPattern (M.toList -> [])))     = [jmacroE| true |]

    toJExpr (PM _ x)                  = error $ "Unimplemented " ++ show x

instance ToJExpr Literal where
    toJExpr (StringLiteral s) = toJExpr s
    toJExpr (IntLiteral s)    = toJExpr s
    toJExpr (DoubleLiteral s) = toJExpr s


