
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


module Formal.TypeCheck where
import List (nub, (\\), intersect, union, partition)
import Monad (msum)

import qualified Data.Map as M

import Data.Monoid

type Id = String

enumId :: Int -> Id
enumId n = "v" ++ show n

data Kind = Star | KindFunction Kind Kind deriving Eq

type Key = String

data Type = TypeVar TypeVar
          | Type TypeConst
          | TypeApplication Type Type
          | TypeRecord TypeRecord
          | TypeGen Int -- (?)
            deriving Eq

data TypeRecord = TRecord (M.Map Key Type) Kind
                deriving Eq

data TypeVar = TVar Id Kind
               deriving Eq

data TypeConst = TypeConst Id Kind -- (?)
                 deriving Eq


-- These are primitive type constants (MPJ pg.6)

unit_type = TypeRecord (TRecord mempty Star)
char_type = Type (TypeConst "Char" Star)
num_type  = Type (TypeConst "Num" Star)
fun_type  = Type (TypeConst "(->)"  (KindFunction Star (KindFunction Star Star)))

array_type  = Type (TypeConst "Array" (KindFunction Star Star))
string_type = TypeApplication array_type char_type


infixr 4 -:>
(-:>) :: Type -> Type -> Type
a -:> b = TypeApplication (TypeApplication fun_type a) b

class HasKind t where kind :: t -> Kind

instance HasKind TypeVar where   kind (TVar _ k) = k
instance HasKind TypeConst where kind (TypeConst _ k) = k
instance HasKind TypeRecord where kind (TRecord _ k) = k
instance HasKind Type where
    kind (Type x) = kind x
    kind (TypeVar x) = kind x
    kind (TypeRecord x) = kind x
    kind (TypeApplication (kind -> KindFunction _ k)  _) = k

type Substitution = [(TypeVar, Type)]

(|->) :: TypeVar -> Type -> Substitution
x |-> y = [(x, y)]

class Types t where
    apply :: Substitution -> t -> t
    tv :: t -> [TypeVar]

instance Types Type where
    apply s (TypeVar u) = case lookup u s of 
                            Just x -> x
                            Nothing -> TypeVar u
    apply s (TypeApplication l r) = TypeApplication (apply s l) (apply s r)
    apply s (TypeRecord (TRecord xs k)) = TypeRecord (TRecord (fmap (apply s) xs) k)
    apply _ t = t

    tv (TypeVar u) = [u]
    tv (TypeApplication l r) = tv l `union` tv r
    tv (TypeRecord (TRecord xs _)) = nub $ M.elems xs >>= tv

instance Types a => Types [a] where
    apply s = map (apply s)
    tv = nub . concat . map tv

infixr 4 @@
(@@) :: Substitution -> Substitution -> Substitution
s1 @@ s2 = ([(u, apply s1 t) | (u, t) <- s2] ++ s1)

merge :: Monad m => Substitution -> Substitution -> m Substitution
merge s1 s2 = if agree then return (s1 ++ s2) else fail "merge fails"
    where agree = all f (map fst s1 `intersect` map fst s2)
          f (TypeVar -> v) = apply s1 v == apply s2 v

mgu :: Monad m => Type -> Type -> m Substitution
var_bind :: Monad m => TypeVar -> Type -> m Substitution



