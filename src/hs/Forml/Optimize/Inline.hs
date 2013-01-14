
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

module Forml.Optimize.Inline where
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
import qualified Forml.Javascript.Utils as J

import Prelude hiding (curry)
import Text.Parsec.Pos (newPos)

import GHC.Generics

data Inlineable = InlineSymbol Symbol | InlineRecord (Expression Definition) deriving (Eq, Generic)

type Inlines = [((Namespace, Inlineable), (Match (Expression Definition), Expression Definition))]
type Inline  = [(Inlineable, (Match (Expression Definition), Expression Definition))]

instance Serialize Inlineable

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


inline_apply pss args opt_args ex = do

    replace_expr (concat $ zipWith gen_expr pss opt_args) (gen_exprs args pss) ex

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

    --optimize (ApplyExpression f' args ) =
    --    ApplyExpression <$> optimize f' <*> mapM optimize args

    --optimize a @ (AccessorExpression x xs) =

    --    do is <- get_env
    --       case (InlineRecord a) `lookup` is of
    --         Just (m @ (Match pss _), ex) ->

    --             do ex'   <- optimize ex
    --                m'    <- optimize m
    --                return $ FunctionExpression [EqualityAxiom m' (Addr undefined undefined ex')]

    --         _ -> flip AccessorExpression xs <$> optimize x

    --optimize (SymbolExpression f) =

    --    do is <- get_env
    --       case (InlineSymbol f) `lookup` is of
    --         Just (m @ (Match pss _), ex) ->

    --             do ex'   <- optimize ex
    --                m'    <- optimize m
    --                return $ FunctionExpression [EqualityAxiom m' (Addr undefined undefined ex')]

    --         _ -> return $ SymbolExpression f

    --optimize (ApplyExpression f args) = ApplyExpression <$> optimize f <*> mapM optimize args
    --optimize (IfExpression a b c) = IfExpression <$> optimize a <*> optimize b <*> optimize c
    --optimize (LazyExpression x l) = flip LazyExpression l <$> optimize x
    --optimize (FunctionExpression xs) = FunctionExpression <$> mapM optimize xs
    --optimize (ListExpression ex) = ListExpression <$> mapM optimize ex

    --optimize (LetExpression ds ex) = do

    --    stmts' <- mapM optimize (sorted_defs . map DefinitionStatement $ ds)
    --    let stmts = map (\(DefinitionStatement d) -> d) stmts'
    --    LetExpression (filter is_inline stmts) <$> optimize ex

    --    where

    --        is_inline (Definition _ True _ _) = False
    --        is_inline _ = True

    --optimize (RecordExpression (M.toList -> xs)) =

    --    let (keys, vals) = unzip xs
    --    in  RecordExpression . M.fromList . zip keys <$> mapM optimize vals

    --optimize (JSExpression x) = return $ JSExpression x
    --optimize (LiteralExpression x) = return $ LiteralExpression x

