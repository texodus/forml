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
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Formal.Javascript (render, render_spec) where

import Language.Javascript.JMacro
import Prelude hiding (curry, (++))

import Formal.Types.Statement

import qualified Data.Map as M

import Formal.Parser
import Formal.Javascript.Utils

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
    comp (WhileStat a b)     = WhileStat (comp a) (comp b)
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

render :: Program -> String
render (Program xs) = show . renderJs $ (comp . (prelude ++) . toStat . map (empty_meta Library xs) $ xs)

render_spec :: Program -> String
render_spec (Program xs) = show . renderJs . (prelude ++) . wrap . toStat . map (empty_meta Test xs) $ xs
    where wrap x = [jmacro| describe("", function() { `(x)`; }); |]

