{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Formal.Types.Axiom where

import Text.InterpolatedString.Perl6
import Language.Javascript.JMacro

import qualified Data.Map as M

import Formal.Parser.Utils
import Formal.Javascript.Utils

import Formal.Types.Type
import Formal.Types.Pattern


import Data.Monoid

import Prelude hiding (curry, (++))


-- Axiom
-- --------------------------------------------------------------------------------

data Axiom a = TypeAxiom UnionType
             | EqualityAxiom (Match a) (Addr a)

instance (Show a) => Show (Axiom a) where
    show (TypeAxiom x) = ": " ++ show x
    show (EqualityAxiom ps ex) = [qq|$ps = $ex|]

instance (Show a, ToJExpr a) => ToJExpr [Axiom a] where
    toJExpr [] = toJExpr . scope $ (Curried [] :: Curried a)
    toJExpr (TypeAxiom _:xs) = toJExpr xs
    toJExpr xs @ (EqualityAxiom (Match ps _) _ : _) = scope . curry (length ps) . toStat . Curried $ xs

newtype Curried a = Curried [Axiom a]

instance (Show a, ToJExpr a) => ToStat (Curried a) where
    toStat (Curried []) = [jmacro| args = []; exhaust(); |]
    toStat (Curried (EqualityAxiom (Match pss cond) (Addr _ _ ex) : xss)) = 

        [jmacro| `(declare_bindings pss)`;
                 if (`(pss)` && `(cond)`) {
                     return `(ex)`;
                 } else `(Curried xss)`; |]


            where declare_bindings [] = mempty
                  declare_bindings (VarPattern x : zs) = declare x [jmacroE| null |] ++ declare_bindings zs
                  declare_bindings (RecordPattern x : zs) = 
                      let (_, z) = unzip . M.toList $ x
                      in  declare_bindings z ++ declare_bindings zs

                  declare_bindings (_ : zs) = declare_bindings zs

instance (ToJExpr a) => ToJExpr (Maybe a) where
    toJExpr = maybe (toJExpr True) toJExpr
