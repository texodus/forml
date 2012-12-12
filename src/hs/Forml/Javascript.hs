{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             MultiParamTypeClasses, NamedFieldPuns, OverlappingInstances,
             QuasiQuotes, RankNTypes, RecordWildCards, TypeSynonymInstances,
             UndecidableInstances #-}

module Forml.Javascript (render, render_spec) where

import Language.Javascript.JMacro
import Prelude                    hiding (curry, (++))

import Forml.Types.Statement

import qualified Data.Map as M

import Forml.Javascript.Backend
import Forml.Parser



class Compress a where
    comp :: a -> a

instance (Compress a, Compress b) => Compress (a, b) where
    comp (a, b) = (comp a, comp b)

instance (Compress a) => Compress [a] where
    comp x = map comp x

instance (Functor m, Compress a) => Compress (m a) where
    comp x = fmap comp x

instance Compress JStat where
    comp (ReturnStat x)      = ReturnStat (comp x)
    comp (IfStat a b c)      = IfStat (comp a) (comp b) (comp c)
    comp (WhileStat a b c)   = WhileStat a (comp b) (comp c)
    comp (ForInStat a b c d) = ForInStat a b (comp c) (comp d)
    comp (SwitchStat a b c)  = SwitchStat (comp a) (comp b) (comp c)
    comp (TryStat a b c d)   = TryStat (comp a) b (comp c) (comp d)
    comp (BlockStat xs)      = BlockStat (comp xs)
    comp (ApplStat a b)      = ApplStat (comp a) (comp b)
    comp (PPostStat a b c)   = PPostStat a b (comp c)
    comp (AssignStat a b)    = AssignStat (comp a) (comp b)
    comp (UnsatBlock a)      = UnsatBlock (comp a)

    comp x = x

    -- comp (DeclStat    Ident (Maybe JLocalType)
    -- comp (UnsatBlock (IdentSupply JStat)
    -- comp (AntiStat   String
    -- comp (ForeignStat Ident JLocalType
    -- comp (BreakStat

instance Compress JVal where
    comp (JList xs)   = JList (comp xs)
    comp (JHash m)    = JHash (M.map comp m)
    comp (JFunc xs x) = JFunc xs (comp x)
    comp (UnsatVal x) = UnsatVal (comp x)

    comp x = x

    -- comp x@(JVar _) = x
    -- comp x@(JDouble _) = x
    -- comp x@(JInt _) = x
    -- comp x@(JStr _) = x
    -- comp x@(JRegEx _) = x

instance Compress JExpr where
    comp (SelExpr e (StrI i))  = IdxExpr (comp e) (ValExpr (JStr i))  -- Closure fix - advanced mode nukes these
    comp (IdxExpr a b)         = IdxExpr (comp a) (comp b)
    comp (InfixExpr a b c)     = InfixExpr a (comp b) (comp c)
    comp (PPostExpr a b c)     = PPostExpr a b (comp c)
    comp (IfExpr a b c)        = IfExpr (comp a) (comp b) (comp c)
    comp (NewExpr a)           = NewExpr (comp a)

   -- comp (ApplExpr (ValExpr (UnsatVal (JFunc [z] y))) [x])

    comp (ApplExpr a b)        = ApplExpr (comp a) (map comp b)
    comp (TypeExpr a b c)      = TypeExpr a (comp b) c
    comp (ValExpr a)           = ValExpr (comp a)
    comp (UnsatExpr a)         = UnsatExpr (comp a)
    comp _ = error "Compression"

render :: Program -> String -> Program -> String
render (Program ys) src (Program xs) = show . renderJs $ (comp . runJS src . toJS . map (empty_meta Library ys) $ xs)

render_spec :: Program -> String -> Program -> String
render_spec (Program ys) src (Program xs) = show . renderJs . wrap . runJS src . toJS . map (empty_meta Test ys) $ xs
    where wrap x = [jmacro| describe("", function() { `(x)`; }); |]



