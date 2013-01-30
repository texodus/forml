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
{-# LANGUAGE UndecidableInstances, KindSignatures, DeriveGeneric #-}

module Forml.TypeCheck.Types where

import Data.List ((\\), nub, intersect, union)

import Text.InterpolatedString.Perl6

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Control.Monad
import Control.Arrow
import Data.Monoid
import Data.Serialize

import GHC.Generics

import Forml.Types.Namespace hiding (Module)

type Id = String

enumId :: Int -> Id
enumId n = 'v' : show n

data Kind = Star | KindFunction Kind Kind deriving (Show, Eq, Ord, Generic)

type Key = String

data Type = TypeVar TypeVar
          | Type TypeConst
          | TypeApplication Type Type
          | TypeRecord TypeRecord
          | TypeGen Int
            deriving (Eq, Generic)

data TPartial = TComplete | TPartial Type deriving (Eq, Show, Generic)

data TypeRecord = TRecord (M.Map Key Type) TPartial Kind
                deriving (Show, Eq, Generic)

data TypeVar = TVar Id Kind
               deriving (Show, Eq, Ord, Generic)

data TypeConst = TypeConst Id Kind -- (?)
                 deriving (Show, Eq, Ord, Generic)

instance Serialize Kind
instance Serialize Type
instance Serialize TPartial
instance Serialize TypeRecord
instance Serialize TypeVar
instance Serialize TypeConst

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

merge :: Monad m => Substitution -> Substitution -> m Substitution
merge s1 s2 = if agree then return (s1 ++ s2) else fail $ "merge fails " ++ show s1 ++ show s2
    where agree = all f (map fst s1 `intersect` map fst s2)
          f (TypeVar -> v) = apply s1 v == apply s2 v







-- Unification
-- --------------------------------------------------------------------------------

instance Monoid TypeVar where
    mappend (TVar x k) (TVar y k') | k == k' = TVar (x ++ y) k
    mempty = TVar "" Star

var_bind :: Monad m => TypeVar -> Type -> m Substitution
match    :: Monad m => Type -> Type -> m Substitution

class Unify a where
    (|=|) :: Monad m => a -> a -> m Substitution

instance Unify TypeRecord where

    TRecord t TComplete _ |=| TRecord u TComplete _
       | M.keysSet t == M.keysSet u = 

            f (M.elems t) (M.elems u) []

        where f [] _ s = return s
              f (x:xs) (y:ys) s = do s' <- apply s x |=| apply s y
                                     f xs ys (s @@ s')

    TRecord t (TPartial (TypeVar p)) k |=| TRecord u (TPartial (TypeVar p')) k' =

        do a <- TRecord t' TComplete k |=| TRecord u' TComplete k'
           b <- f (u M.\\ t) p  (p `mappend` p') k
           c <- f (t M.\\ u) p' (p `mappend` p') k
           return$ a @@ b @@ c

        where t' = u M.\\ (u M.\\ t)
              u' = t M.\\ (t M.\\ u)
              
              f x p p' k | M.size x > 0 = var_bind p (TypeRecord (TRecord x (TPartial (TypeVar p')) k))
                         | otherwise    = return []

    t' @ (TRecord t (TPartial (TypeVar p)) k) |=| u' @ (TRecord u TComplete k')
        | M.keysSet t `S.intersection` M.keysSet u == M.keysSet t =

            do a <- TRecord t TComplete k |=| TRecord (u M.\\ (u M.\\ t)) TComplete k'
               b <- if M.size u >= M.size t
                    then var_bind p$ TypeRecord$ TRecord (u M.\\ t) TComplete Star
                    else return []
               return$ a @@ b

        | otherwise = fail [qq|Records do not unify: found {TypeRecord t'}, expecting {TypeRecord u'}|]
                    
    t |=| u @ (TRecord _ (TPartial _) _) = u |=| t

    t |=| u = fail [qq|Illegal record unification: {TypeRecord t} and {TypeRecord u}|]

instance Unify Type where

    TypeApplication l1 r1 |=| TypeApplication l2 r2 =

        do s1 <- l1 |=| l2
           s2 <- apply s1 r1 |=| apply s1 r2
           return$ s1 @@ s2

    TypeVar u |=| t = var_bind u t
    t |=| TypeVar u = var_bind u t
    TypeRecord a |=| TypeRecord b = a |=| b
    Type t |=| Type u | t == u = return []
    t |=| u = fail$ "Types do not unify: found " ++ show t ++ ", expecting " ++ show u

data Z a = Z a | Error String

instance Monad Z where
    return = Z
    (Z x) >>= f = f x
    (Error x) >>= _ = Error x
    fail = Error

mgu :: Type -> Type -> TI Substitution
mgu x y = do z <- mgu' x y
             case z of
               Left e  -> add_error e >> return []
               Right e -> return e

mgu' :: Type -> Type -> TI (Either String Substitution)
mgu' x y = case x |=| y of
             Z z -> return $ Right z
             Error e -> return $ Left e

var_bind u t | t == TypeVar u   = return []
             | u `elem` tv t    = fail $ "occurs check fails: " ++ show u ++ show t
             | kind u /= kind t = fail "kinds do not match"
             | otherwise        = return (u +-> t)

match (TypeApplication l1 r1) (TypeApplication l2 r2) =
    do s1 <- match l1 l2
       s2 <- match r1 r2
       merge s1 s2

match (TypeVar u) t 
    | kind u == kind t = return (u +-> t)
match (TypeRecord t) (TypeRecord u) 
    | t == u = return []
match (Type t) (Type u)
    | t == u = return []
match _ _ = fail "Types do not match"




-- Type Schemes
-- --------------------------------------------------------------------------------

data Scheme = Forall [Kind] Type deriving (Eq, Generic)

instance Serialize Scheme

instance Show Scheme where
    show (Forall [] t) = show t
    show (Forall xs t) = show t --"∀" ++ vars ++ " ⇒ " ++ show t
        where vars = concat . L.intersperse " " . map (\x -> (map (:[]) "abcdefghijklmnopqrstuvwxyz") !! x) . take (length xs) $ [0..]

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall _ qt)      = tv qt

quantify      :: [TypeVar] -> Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
 where vs' = [ v | v <- tv qt, v `elem` vs ]
       ks  = map kind vs'
       s   = zip vs' (map TypeGen [0..])

toScheme      :: Type -> Scheme
toScheme t     = Forall [] t



-- Assumptions
-- --------------------------------------------------------------------------------

data Assumption = Id :>: Scheme | Scheme :>>: Scheme deriving (Eq, Generic)

instance Serialize Assumption

newtype A = A [Assumption]

instance Show Assumption where
    show (i :>: s)  = i ++ ": " ++ show s
    show (i :>>: s) = show i ++ ": " ++ show s

instance Types Assumption where
    apply s (i :>: sc)  = i :>: (apply s sc)
    apply s (sc :>>: sd) = apply s sc :>>: apply s sd

    tv (_ :>: sc)       = tv sc
    tv (sc :>>: _) = tv sc

class Find a b | a -> b where
    find :: a -> TI b

instance Find Id Scheme where

    find i = do (reverse -> x) <- get_assumptions
                find' x 

        where find' ((i':>:sc):as) = if i == i' then return sc else find' as
              find' (_:as)         = find' as
              find' []             = do add_error ("Unbound identifier " ++ show i)
                                        return$ toScheme$ TypeVar (TVar "a" Star)
              

instance Find Scheme (Maybe (Scheme, Scheme)) where

    find i = do (reverse -> x) <- get_assumptions
                find' x 

        where find' []              = return Nothing
              find' ((i':>>:sc):as) = do i''  <- freshInst i
                                         i''' <- freshInst i'
                                         x    <- i'' `can_unify` i'''
                                         if x then return $ Just (i', sc) else find' as
              find' (_:as)          = find' as
              
instance Find Type (Maybe (Substitution, Scheme)) where

    find x = do as <- get_assumptions
                find''' as x

             where find''' [] _ = return Nothing
                   find''' (_:>:_:xs) t = find''' xs t
                   find''' ((Forall _ x :>>: y):xs) t =
                   
                       do t' <- return$ inst (map TypeVar$ tv t) x
                          case t' |=| t of
                            Error _ -> find''' xs t
                   
                            -- TODO Only allow this shorthand if the match is unique - true?
                            Z x  -> do zz' <- find''' xs t
                                       case zz' of
                                         Nothing -> return$ Just (x, y)
                                         Just _ -> return$ Nothing




-- Type Inference Monad
-- --------------------------------------------------------------------------------

data TIState = TIState { substitution :: Substitution
                       , seed         :: Int
                       , msg          :: String
                       , warnings     :: [String]
                       , errors       :: [String]
                       , modules      :: [(Namespace, [Assumption])]
                       , namespace    :: Namespace
                       , assumptions  :: [Assumption] }

newtype TI a = TI (TIState -> (TIState, a))

instance Monad TI where
  fail x     = TI (\y -> error$ x ++ "\n" ++ msg y) 
  return x   = TI (\y -> (y, x))
  TI f >>= g = TI (\x -> case f x of
                           (y, x') -> let TI gx = g x'
                                      in  gx y)

instance Functor TI where
    fmap f y = y >>= (\x -> TI (\x' -> (x', f x)))

instance Types [(Namespace, [Assumption])] where
    apply = map . second . apply
    tv = concatMap (\(_, y) -> tv y)


runTI :: TI a -> a
runTI (TI f) = x where (_, x) = f (TIState [] 0 "" [] [] [] (Namespace []) [])

get_assumptions  :: TI [Assumption]
get_msg          :: TI String
set_msg          :: String -> TI ()
get_substitution :: TI Substitution
add_error        :: String -> TI ()

get_assumptions  = TI (\x -> (x, assumptions x))
get_msg          = TI (\x -> (x, msg x))
set_msg x        = TI (\y -> (y { msg = x }, ()))
get_substitution = TI (\x -> (x, substitution x))
add_error y      = TI (\x -> (x { errors = errors x ++ [y ++ "\n" ++ msg x] }, ()))
set_assumptions :: [Assumption] -> TI ()
get_errors :: TI [String]
get_errors       = TI (\x -> (x, errors x))
get_modules :: TI [(Namespace, [Assumption])]
get_modules      = TI (\x -> (x, modules x))
set_assumptions x = TI (\y -> (y { assumptions = x }, ()))
get_namespace :: TI Namespace
get_namespace = TI (\y -> (y, namespace y))
          
class Assume a where
    assume :: a -> TI ()

instance Assume Assumption where
    assume x = TI (\y -> (y { assumptions = assumptions y ++ (x:[]) }, ()))

instance Assume [Assumption] where
    assume x = TI (\y -> (y { assumptions = assumptions y ++ x}, ()))

with_scope :: TI a -> TI a
with_scope x = do as <- get_assumptions
                  y  <- x
                  set_assumptions as
                  return y

-- TODO
with_module :: String -> TI [Assumption] -> TI ()
with_module name x = do --as <- get_assumptions
                        ns <- get_namespace
                        set_namespace (ns `mappend` Namespace [name])
                        as' <- x
                        --as' <- get_assumptions
                        --set_assumptions as
                        add_module (ns `mappend` Namespace [name], as')
                        set_namespace ns

    where add_module x = TI (\y -> (y { modules = modules y ++ [x] }, ()))

          set_namespace x = TI (\y -> (y { namespace = x }, ()))


substitute :: Types a => a -> TI a
substitute x = do y <- get_substitution
                  return $ apply y x

unify :: Type -> Type -> TI ()
unify t1 t2 = do s <- get_substitution
                 u <- apply s t1 `mgu` apply s t2
                 extSubst $ u @@ s

    where extSubst   :: Substitution -> TI ()
          extSubst s' = TI (\x -> (x { substitution = s' }, ()))

can_unify :: Type -> Type -> TI Bool
can_unify t1 t2 = with_scope $ 
              do s <- get_substitution
                 u <- apply s t1 `mgu'` apply s t2
                 case u of
                   Left x  -> return False
                   Right x -> return True

newTVar :: Kind -> TI Type
newTVar k   = TI (\x -> let v = TVar (enumId (seed x)) k
                       in  (x { seed = seed x + 1}, TypeVar v))

freshInst :: Scheme -> TI Type
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

class Instantiate t where
  inst  :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TypeApplication l r) = TypeApplication (inst ts l) (inst ts r)
  inst ts (TypeGen n)  = ts !! n
  inst ts (TypeRecord (TRecord m TComplete k)) =
      TypeRecord (TRecord (M.map (inst ts) m) TComplete k)
  inst ts (TypeRecord (TRecord m (TPartial p) k)) =
      TypeRecord (TRecord (M.map (inst ts) m) (TPartial (inst ts p)) k)
  inst _ t   = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

class Infer e t | e -> t where infer :: e -> TI t

list_scheme :: Scheme
list_scheme = Forall [Star] qual_list
    where qual_list = TypeApplication (Type (TypeConst "List" (KindFunction Star Star))) (TypeGen 0)

bool_type :: Type
bool_type = Type (TypeConst "Bool" Star)

-- Expressions


infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TypeApplication (TypeApplication fun_type a) b
