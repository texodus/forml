{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Forml.TypeCheck.Types where

import System.IO.Unsafe

import Data.List ((\\), nub, intersect, union)

import Text.InterpolatedString.Perl6

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Control.Monad
import Control.Arrow
import Data.Monoid

import Forml.Types.Namespace hiding (Module)

type Id = String

enumId :: Int -> Id
enumId n = 'v' : show n

data Kind = Star | KindFunction Kind Kind deriving (Show, Eq, Ord)

type Key = String

data Type = TypeVar TypeVar
          | Type TypeConst
          | TypeApplication Type Type
          | TypeRecord TypeRecord
          | TypeGen Int
            deriving Eq

data TPartial = TComplete | TPartial Type deriving (Eq, Show)

data TypeRecord = TRecord (M.Map Key Type) TPartial Kind
                deriving (Show, Eq)

data TypeVar = TVar Id Kind
               deriving (Show, Eq, Ord)

data TypeConst = TypeConst Id Kind -- (?)
                 deriving (Show, Eq, Ord)

instance Show Type where
    show (TypeVar (TVar i _)) = i
    show (Type (TypeConst i _)) = i
    show (TypeApplication (TypeApplication (Type (TypeConst "->" _)) 
                          u @ (TypeApplication (TypeApplication (Type (TypeConst "->" _)) _) _)) v) = 
        "(" ++ show u ++ ") → " ++ show v
    show (TypeApplication (TypeApplication (Type (TypeConst "->" _)) u) v) = 
        show u ++ " → " ++ show v
    show (TypeApplication t u) = show t ++ " " ++ show u
    show (TypeGen x) = (map (:[])$ concat$ repeat "abcdefghijklmnopqrstuvwxyz") !! x
    show (TypeRecord (TRecord m TComplete _)) =
        "{" ++ (concat$ L.intersperse ", "$ map (\(x, y) -> x ++ " = " ++ show y) . M.toList $ m) ++ "}"
    show (TypeRecord (TRecord m (TPartial _) _)) =
        "{" ++ (concat$ L.intersperse ", "$ map (\(x, y) -> x ++ " = " ++ show y) . M.toList $ m) ++ ", _ }"


num_type :: Type
fun_type :: Type
num_type  = Type (TypeConst "Num" Star)
fun_type  = Type (TypeConst "->" (KindFunction Star (KindFunction Star Star)))

infixr 4 -:>
(-:>) :: Type -> Type -> Type
a -:> b = TypeApplication (TypeApplication fun_type a) b

class HasKind t where kind :: t -> Kind

instance HasKind TypeVar where   kind (TVar _ k) = k
instance HasKind TypeConst where kind (TypeConst _ k) = k
instance HasKind TypeRecord where kind (TRecord _ _ k) = k
instance HasKind Type where
    kind (Type x) = kind x
    kind (TypeVar x) = kind x
    kind (TypeRecord x) = kind x
    kind (TypeApplication (kind -> KindFunction _ k)  _) = k
    kind t = error$ "Could not determine kind of " ++ show t

type Substitution = [(TypeVar, Type)]

(+->) :: TypeVar -> Type -> Substitution
x +-> y = [(x, y)]

class Types t where
    apply :: Substitution -> t -> t
    tv :: t -> [TypeVar]

instance Types Type where
    apply s (TypeVar u) = case lookup u s of 
                            Just x -> x
                            Nothing -> TypeVar u
    apply s (TypeApplication l r) = TypeApplication (apply s l) (apply s r)
    apply s (TypeRecord (TRecord xs TComplete k)) = TypeRecord (TRecord (fmap (apply s) xs) TComplete k)
    apply s (TypeRecord (TRecord xs (TPartial p) k)) =
        case apply s p of
          p' | p' == p   -> TypeRecord (TRecord (fmap (apply s) xs) (TPartial p) k)
          TypeRecord (TRecord ys p' _) -> TypeRecord (TRecord (fmap (apply s) (xs `M.union` ys)) p' k)
          x -> TypeRecord (TRecord (fmap (apply s) xs) (TPartial x) k)
    apply _ t = t

    tv (TypeVar u) = [u]
    tv (TypeApplication l r) = tv l `union` tv r
    tv (TypeRecord (TRecord xs TComplete _)) = nub $ M.elems xs >>= tv
    tv (TypeRecord (TRecord xs (TPartial t) _)) = (nub $ M.elems xs >>= tv) ++ tv t
    tv _ = []

instance Types a => Types [a] where
    apply s = map (apply s)
    tv = nub . concatMap tv

infixr 4 @@
(@@) :: Substitution -> Substitution -> Substitution
s1 @@ s2 = ([(u, apply s1 t) | (u, t) <- s2] ++ s1)


instance Monoid TypeVar where
    mappend (TVar x k) (TVar y k') | k == k' = TVar (x ++ y) k
    mempty = TVar "" Star




