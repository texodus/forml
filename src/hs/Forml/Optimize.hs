
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
import qualified Forml.Javascript.Utils as J

import Prelude hiding (curry)
import Text.Parsec.Pos (newPos)

import GHC.Generics

data Inlineable = InlineSymbol Symbol | InlineRecord (Expression Definition) deriving (Eq, Generic)

type Inlines = [((Namespace, Inlineable), (Match (Expression Definition), Expression Definition))]
type Inline  = [(Inlineable, (Match (Expression Definition), Expression Definition))]

data OptimizeState = OptimizeState { ns :: Namespace
                                   , assumptions :: [(Namespace, [Assumption])]
                                   , inlines :: Inlines
                                   , tco :: [String]
                                   , env :: Inline } deriving (Eq, Generic)

data Optimizer a = Optimizer (OptimizeState -> (OptimizeState, a))

instance Serialize Inlineable
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

get_addr :: Addr a -> a
get_addr (Addr _ _ x) = x


afmap d f (EqualityAxiom (Match pss cond) expr) =
    EqualityAxiom (Match pss (fmap (replace_expr d f) cond)) (fmap (replace_expr d f) expr)

afmap _ _ t = t

dfmap d f (Definition a b c as) =
    Definition a b c (fmap (afmap d f) as)

replace_expr d f (ApplyExpression a b) =
    ApplyExpression (replace_expr d f a) (replace_expr d f `map` b)
replace_expr d f (IfExpression a b Nothing) =
    IfExpression (replace_expr d f a) (replace_expr d f b) Nothing
replace_expr d f (IfExpression a b (Just c)) =
    IfExpression (replace_expr d f a) (replace_expr d f b) (Just (replace_expr d f c))
replace_expr d f (LiteralExpression x) =
    LiteralExpression x
replace_expr d f (JSExpression j) =
    JSExpression (replace_jexpr d j)
replace_expr d f (LazyExpression a b) =
    LazyExpression (fmap (replace_expr d f) a) b
replace_expr d f (FunctionExpression as) = 
    FunctionExpression (map (afmap d f) as)
replace_expr d f (RecordExpression vs) =
    RecordExpression (fmap (replace_expr d f) vs)
replace_expr d f (LetExpression ds e) =
    LetExpression (dfmap d f `map` ds) (replace_expr d f e)
replace_expr d f (ListExpression xs) =
    ListExpression (map (replace_expr d f) xs)
replace_expr d f (AccessorExpression a ss) =
    AccessorExpression (fmap (replace_expr d f) a) ss
replace_expr d f (SymbolExpression (Symbol s)) = f (Symbol s)
replace_expr d f (SymbolExpression s) = SymbolExpression s

replace_expr _ _ s = s



replace_stat dict (ReturnStat x)      = ReturnStat (replace_jexpr dict x)
replace_stat dict (IfStat a b c)      = IfStat (replace_jexpr dict a) (replace_stat dict b) (replace_stat dict c)
replace_stat dict (WhileStat a b c)   = WhileStat a (replace_jexpr dict b) (replace_stat dict c)
replace_stat dict (ForInStat a b c d) = ForInStat a b (replace_jexpr dict c) (replace_stat dict d)
replace_stat dict (SwitchStat a b c)  = SwitchStat (replace_jexpr dict a) b (replace_stat dict c)
replace_stat dict (TryStat a b c d)   = TryStat (replace_stat dict a) b (replace_stat dict c) (replace_stat dict d)
replace_stat dict (BlockStat xs)      = BlockStat (replace_stat dict `map` xs)
replace_stat dict (ApplStat a b)      = ApplStat (replace_jexpr dict a) (replace_jexpr dict `map` b)
replace_stat dict (PPostStat a b c)   = PPostStat a b (replace_jexpr dict c)
replace_stat dict (AssignStat a b)    = AssignStat (replace_jexpr dict a) (replace_jexpr dict b)
replace_stat dict (UnsatBlock a)      = UnsatBlock (replace_stat dict `fmap` a)

replace_stat dict (DeclStat v t) = DeclStat v t
replace_stat dict (UnsatBlock ident_supply) = UnsatBlock (replace_stat dict `fmap` ident_supply)
replace_stat dict (AntiStat s) = AntiStat s
replace_stat dict (ForeignStat s t) = ForeignStat s t
replace_stat dict (BreakStat s) = (BreakStat s)

replace_jval dict (JList xs)   = JList (replace_jexpr dict `map` xs)
replace_jval dict (JHash m)    = JHash (M.map (replace_jexpr dict) m)
replace_jval dict (JFunc xs x) = JFunc xs (replace_stat dict x)
replace_jval dict (UnsatVal x) = UnsatVal (replace_jval dict `fmap` x)
replace_jval dict x@(JDouble _) = x
replace_jval dict x@(JInt _) = x
replace_jval dict x@(JStr _) = x
replace_jval dict x@(JRegEx _) = x
replace_jval dict (JVar (StrI y)) =
    case y `lookup` dict of
         Just y' -> JVar . StrI . show . renderJs . toJExpr $ y'
         Nothing -> JVar (StrI y)
replace_jval _ (JVar x) = JVar x


replace_jexpr dict (SelExpr e (StrI i))  = IdxExpr (replace_jexpr dict e) (ValExpr (JStr i))  -- Closure fix - advanced mode nukes these
replace_jexpr dict (IdxExpr a b)         = IdxExpr (replace_jexpr dict a) (replace_jexpr dict b)
replace_jexpr dict (InfixExpr a b c)     = InfixExpr a (replace_jexpr dict b) (replace_jexpr dict c)
replace_jexpr dict (PPostExpr a b c)     = PPostExpr a b (replace_jexpr dict c)
replace_jexpr dict (IfExpr a b c)        = IfExpr (replace_jexpr dict a) (replace_jexpr dict b) (replace_jexpr dict c)
replace_jexpr dict (NewExpr a)           = NewExpr (replace_jexpr dict a)
replace_jexpr dict (ApplExpr a b)        = ApplExpr (replace_jexpr dict a) (replace_jexpr dict `map` b)
replace_jexpr dict (TypeExpr a b c)      = TypeExpr a (replace_jexpr dict b) c
replace_jexpr dict (ValExpr a)           = ValExpr (replace_jval dict a)
replace_jexpr dict (UnsatExpr a)         = UnsatExpr (replace_jexpr dict `fmap` a)


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
                optimize $ replace_expr (concat $ zipWith gen_expr pss args') (gen_exprs args pss) ex
            _ -> ApplyExpression <$> optimize (SymbolExpression s) <*> mapM optimize args

        where

            gen_exprs args' pats (Symbol s) = 
                case s `lookup` (concat $ zipWith gen_expr pats args') of
                  Just ex -> ex
                  Nothing -> (SymbolExpression (Symbol s))

            gen_exprs _ _ s = SymbolExpression s

            gen_expr (VarPattern x) y =
                [(x, y)]
            gen_expr (RecordPattern xs _) y =
                concat $ zipWith gen_expr (M.elems xs) (map (to_accessor y) $ M.keys xs)
            gen_expr (ListPattern xs) y =
                concat $ zipWith gen_expr xs (map (to_array y) [0..])
            gen_expr (AliasPattern xs) y =
                concatMap (flip gen_expr y) xs
            gen_expr _ _ =
                []

            to_array expr idx =
                JSExpression [jmacroE| `(expr)`[idx] |]

            to_accessor expr sym =
                AccessorExpression (Addr undefined undefined expr) [sym]

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

    optimize (Definition a b name xs) | is_recursive xs =

       do xs' <- mapM optimize xs
          add_tco $ show name
          return $ Definition a b name (axioms xs')

       where is_recursive (TypeAxiom _: xs') = is_recursive xs'
             is_recursive (EqualityAxiom _ x: xs') = is_recursive' (get_addr x) || is_recursive xs'
             is_recursive [] = False

             is_recursive' (ApplyExpression (SymbolExpression x) _) | name == x = True
             is_recursive' (LetExpression _ e) = is_recursive' e
             is_recursive' (IfExpression _ a (Just b)) = is_recursive' a || is_recursive' b
             is_recursive' (IfExpression _ a _) = is_recursive' a
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

          

