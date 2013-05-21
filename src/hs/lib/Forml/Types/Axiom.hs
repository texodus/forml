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
{-# LANGUAGE DeriveGeneric #-}

module Forml.Types.Axiom where

import Text.InterpolatedString.Perl6
import Language.Javascript.JMacro

import qualified Data.Map as M

import Forml.Parser.Utils
import Forml.Javascript.Utils

import Forml.Types.Type
import Forml.Types.Pattern

import GHC.Generics

import Data.Serialize
import Data.Monoid

import Prelude hiding (curry, (++))


-- Axiom
-- --------------------------------------------------------------------------------

data Axiom a = TypeAxiom UnionType
             | EqualityAxiom (Match a) (Addr a)
             deriving (Eq, Generic)

instance (Serialize a) => Serialize (Axiom a)

instance (Show a) => Show (Axiom a) where
    show (TypeAxiom x) = ": " ++ show x
    show (EqualityAxiom ps ex) = [qq|$ps = $ex|]

instance (Show a, ToJExpr a) => ToJExpr [Axiom a] where
    toJExpr [] = toJExpr . scope $ (Curried [] :: Curried a)
    toJExpr (TypeAxiom _:xs) = toJExpr xs
    toJExpr xs @ (EqualityAxiom (Match ps _) _ : _) = scope . curry (length ps) id . toStat . Curried $ xs

newtype Curried a = Curried [Axiom a]

instance (Show a, ToJExpr a) => ToStat (Curried a) where

    toStat (Curried []) = [jmacro| exhaust(); |]

    toStat (Curried (EqualityAxiom (Match [] Nothing) (Addr _ _ ex) : xss)) = 

        [jmacro| return `(ex)`; |]

    toStat (Curried (EqualityAxiom (Match pss Nothing) (Addr _ _ ex) : xss)) = 

        case toJExpr pss of
            ValExpr (JVar (StrI "true")) -> [jmacro|
                `(declare_bindings (var_names pss) pss)`;
                return `(ex)`;
            |]

            InfixExpr "&&" (ValExpr (JVar (StrI "true"))) (ValExpr (JVar (StrI "true"))) -> [jmacro|
                `(declare_bindings (var_names pss) pss)`;
                return `(ex)`;
            |]

            x -> [jmacro|
                `(declare_bindings (var_names pss) pss)`;
                if (`(x)`) return `(ex)`;
                `(Curried xss)`;
            |]
                    
    toStat (Curried (EqualityAxiom (Match [] cond) (Addr _ _ ex) : xss)) = 

        [jmacro|    if (`(cond)`) return `(ex)`; |]
                    

    toStat (Curried (EqualityAxiom (Match pss cond) (Addr _ _ ex) : xss)) = 

        [jmacro|    `(declare_bindings (var_names pss) pss)`;
                    if (`(pss)` && `(cond)`) return `(ex)`;
                    `(Curried xss)`; |]


declare_bindings (name : names) (AliasPattern x : zs) =

    declare_bindings (take (length x) (repeat name)) x ++ declare_bindings names zs

declare_bindings (name : names) (VarPattern x : zs) =
    
    [jmacro| `(declare x name)`; |] ++ declare_bindings names zs

declare_bindings (name : names) (RecordPattern x _: zs) = 
    let (ns, z) = unzip . M.toList $ x
    in  declare_bindings (map (acc name) ns) z ++ declare_bindings names zs

declare_bindings (_ : names) (_ : zs) = declare_bindings names zs
declare_bindings [] [] = mempty

var_names :: forall a. [a] -> [JExpr]
var_names pss = map ref . reverse . take (length pss) . map local_pool $ [0 .. 26]

acc n ns = [jmacroE| `(n)`[`(ns)`] |]


instance (ToJExpr a) => ToJExpr (Maybe a) where
    toJExpr = maybe (toJExpr True) toJExpr
