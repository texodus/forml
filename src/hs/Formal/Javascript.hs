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

module Formal.Javascript (render, render_spec) where

import Text.InterpolatedString.Perl6
import Language.Javascript.JMacro
import Data.Monoid
import qualified Data.Map as M
import qualified Data.List as L
import Formal.Parser.AST
import Formal.Parser.Utils
import Prelude hiding (curry, (++))

prelude :: JStat
prelude = [jmacro| function !is_array(x) { 
                       return `(InfixExpr "instanceof" x (ref "Array"))`;
                   }

                   function !error(x) { throw x; }

                   function !exhaust() { error("Pattern Match Exhausted"); }

                   function !check(x) {
                       result = (typeof x != "undefined");
                       return result;
                   } |]

render :: Program -> String
render (Program xs) = show . renderJs . (prelude ++) . toStat . map (empty_meta Library xs) $ xs

render_spec :: Program -> String
render_spec (Program xs) = show . renderJs . (prelude ++) . toStat . map (empty_meta Test xs) $ xs

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
          build_modules (DefinitionStatement (Definition n _): xs) = Var (to_name n) : build_modules xs
          build_modules (_ : xs) = build_modules xs
          build_modules [] = []

to_name :: Symbol -> String
to_name (Symbol x) = x
to_name (Operator op) = concat . map (\x -> M.findWithDefault "_" x operator_dict) $ op

--------------------------------------------------------------------------------
----
---- Utilities

declare          :: String -> JExpr -> JStat
declare_this     :: String -> JExpr -> JStat
declare_window   :: String -> JExpr -> JStat
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

declare_window name expr =

    [jmacro| `(declare name expr)`;
             (typeof global == "undefined" ? window : global)[`(name)`] = `(ref name)`; |]




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

instance ToJExpr Symbol where
    toJExpr = toJExpr . to_name

instance Open Statement where
    open _ [] = mempty
    open ns (DefinitionStatement (Definition n _) : xs) =

        let f = ref . show
            x = [jmacroE| `(f ns)`[`(n)`] |] in

        [jmacro| `(declare (to_name n) x)`;
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

newtype Jasmine = Jasmine Expression

instance ToStat Jasmine where

    toStat (Jasmine (ApplyExpression (SymbolExpression (Operator "==")) [x, y])) =

        [jmacro| expect(`(x)`).toEqual(`(y)`); |]

    toStat (Jasmine (ApplyExpression (SymbolExpression (Operator "!=")) [x, y])) =

        [jmacro| expect(`(x)`).toNotEqual(`(y)`); |]
        
    toStat (Jasmine e) = 

        [jmacro| expect((function() {
                     try {
                         return `(e)`;
                     } catch (ex) {
                         return { error: ex };
                     }
                 })()).toEqual(true); |]


instance ToStat Meta where

    -- Expressions are ignored for Libraries, and rendered as tests for Test
    toStat (Meta { target = Library, expr = ExpressionStatement _ }) = mempty
    toStat (Meta { target = Test,    expr = ExpressionStatement e }) = 

        [jmacro| it(`(show e)`, function() {
                     `(Jasmine e)`;
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
    toStat meta @ (Meta { target = Library, namespace = Namespace [], expr = ModuleStatement ns xs, .. }) =
        
        let ex = [jmacroE| new (function() { 
                               `(fmap (\z -> meta { namespace =  ns, expr = z }) xs)`;
                           }) |]

        in declare_window (show ns) ex

    toStat meta @ (Meta { target = Library, expr = ModuleStatement ns xs, .. }) =
        
        let ex = [jmacroE| new (function() { 
                               `(fmap (\z -> meta { namespace = namespace ++ ns, expr = z }) xs)`;
                           }) |]

        in declare_this (show ns) ex

    toStat meta @ (Meta { target = Test, expr = ModuleStatement ns xs, .. }) =

        let (imports, rest) = L.partition f xs
            f (ImportStatement _) = True
            f _ = False in

        [jmacro| describe(`(show ns)`, function() {
                     `(map (\z -> meta { namespace = namespace ++ ns, expr = z }) imports)`; 
                     `(open (namespace ++ ns) xs)`;
                     var x = new (function {
                         `(map (\z -> meta { namespace = namespace ++ ns, expr = z }) rest)`; 
                     }());
                 }); |]

    -- Definitions are ignored for the Test target
    toStat (Meta { target = Library, expr = DefinitionStatement d }) = toStat d
    toStat (Meta { target = Test,    expr = DefinitionStatement _ }) = mempty

    toStat (Meta { expr = TypeStatement _ _ }) = mempty

    toStat x = error $ "Unimplemented " ++ show x

instance (ToStat a) => ToStat [a] where
    toStat [] = mempty
    toStat x = foldl1 mappend . map toStat $ x

instance ToStat Definition where
    toStat (Definition name as) = declare_this (to_name name) $ toJExpr as

instance ToStat Local where
    toStat (Local (Definition name as)) = declare (to_name name) $ toJExpr as


instance ToJExpr Namespace where
    toJExpr (Namespace []) = ref "this"
    toJExpr (Namespace (end -> x : xs)) = [jmacroE| `(Namespace xs)`[`(x)`] |]

instance ToJExpr [Axiom] where
    toJExpr [] = toJExpr . scope . Curried $ []
    toJExpr (TypeAxiom _:xs) = toJExpr xs
    toJExpr xs @ (EqualityAxiom (Match ps _) _ : _) = scope . curry (length ps) . toStat . Curried $ xs

instance ToStat Curried where
    toStat (Curried []) = [jmacro| args = []; exhaust(); |]
    toStat (Curried (EqualityAxiom (Match pss cond) ex : xss)) = 

        [jmacro| `(declare_bindings pss)`;
                 if (`(pss)` && `(cond)`) {
                     return `(ex)`;
                 } else `(Curried xss)`; |]

instance ToJExpr [Pattern] where
    toJExpr ps = toJExpr $ zipWith PM (reverse . take (length ps) . map local_pool $ [0 .. 26]) ps

instance ToJExpr (Maybe Expression) where
    toJExpr = maybe (toJExpr True) toJExpr

newtype Local = Local Definition

instance ToJExpr Expression where

    -- These are inline cheats to improve performance
    toJExpr (ApplyExpression (SymbolExpression (Operator "==")) [x, y]) = [jmacroE| _eq_eq(`(x)`)(`(y)`) |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "!=")) [x, y]) = [jmacroE| !_eq_eq(`(x)`)(`(y)`) |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "+")) [x, y])  = [jmacroE| `(x)` + `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "*")) [x, y])  = [jmacroE| `(x)` * `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "-")) [x, y])  = [jmacroE| `(x)` - `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "&&")) [x, y]) = [jmacroE| `(x)` && `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "||")) [x, y]) = [jmacroE| `(x)` || `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "<=")) [x, y]) = [jmacroE| `(x)` <= `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator ">=")) [x, y]) = [jmacroE| `(x)` >= `(y)` |]

    toJExpr (ApplyExpression (SymbolExpression f @ (Operator _)) [x, y])    = toJExpr (ApplyExpression (SymbolExpression (Symbol (to_name f))) [x,y])
    toJExpr (ApplyExpression (SymbolExpression (Operator f)) x)        = error $ "Operator with " ++ show (length x) ++ " params"
    toJExpr (ApplyExpression (SymbolExpression (Symbol f)) [])        = ref f
    toJExpr (ApplyExpression f []) = [jmacroE| `(f)` |]
    toJExpr (ApplyExpression f (end -> x : xs)) = [jmacroE| `(ApplyExpression f xs)`(`(x)`) |]

    toJExpr (ListExpression x)      = toJExpr x
    toJExpr (LiteralExpression l)   = toJExpr l
    toJExpr (SymbolExpression (Symbol x))    = ref x
    toJExpr (NamedExpression x y)   = [jmacroE| (function(){ var g = {}; g[`(x)`] = `(y)`; return g})() |]
    toJExpr (IfExpression x y z)    = [jmacroE| (function(){ if (`(x)`) { return `(y)`; } else { return `(z)` }})() |] 
    toJExpr (FunctionExpression x)  = toJExpr x
    toJExpr (RecordExpression m)    = toJExpr (M.mapKeys show m)
    toJExpr (LetExpression bs ex)   = [jmacroE| (function() { `(map Local bs)`; return `(ex)` })() |]
    toJExpr (JSExpression s)        = s
    toJExpr x                       = error $ "Unimplemented " ++ show x

instance ToJExpr [PatternMatch] where
    toJExpr []     = toJExpr True
    toJExpr (x:xs) = [jmacroE| `(x)` && `(xs)` |]

instance ToJExpr PatternMatch where
    toJExpr (PM _ AnyPattern)                       = toJExpr True
    toJExpr (PM n (VarPattern x))                   = [jmacroE| (function() { if (typeof `(ref n)` != "undefined") { `(ref x)` = `(ref n)`; return true; } else return false })() |]
    toJExpr (PM n (LiteralPattern x))               = [jmacroE| `(ref n)` === `(x)` |]
    toJExpr (PM n (RecordPattern (M.toList -> xs))) = [jmacroE| check(`(ref n)`) && `(map (\(key, val) -> (PM (n ++ "[\"" ++ to_name key ++ "\"]") val)) xs)` |]
    toJExpr (PM n (RecordPattern (M.toList -> []))) = [jmacroE| true |]
    toJExpr (PM n (NamedPattern x Nothing))         = [jmacroE| !!`(ref n)`[`(x)`] |]
    toJExpr (PM n (NamedPattern x (Just y)))        = toJExpr (PM n (RecordPattern (M.fromList [(Symbol x, y)])))
    toJExpr (PM n (ListPattern []))                 = [jmacroE| equals(`(n)`)([]) |]
    toJExpr (PM n (ListPattern xs))                 = 
        let x = toJExpr (map (\(index, val) -> toJExpr (PM (n ++ "[" ++ show index ++ "]") val)) (zip [0..] xs))
        in   [jmacroE| `(x)` && `(ref n)`.length == `(length xs)` |]
    toJExpr (PM _ x)                  = error $ "Unimplemented " ++ show x

instance ToJExpr Literal where
    toJExpr (StringLiteral s) = toJExpr s
    toJExpr (IntLiteral s)    = toJExpr s
    toJExpr (DoubleLiteral s) = toJExpr s


