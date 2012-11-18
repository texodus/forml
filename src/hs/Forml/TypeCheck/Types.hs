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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, KindSignatures #-}

module Forml.TypeCheck.Types where

import Data.List (nub, intersect, union)
import System.IO.Unsafe

import Text.InterpolatedString.Perl6

import Data.List (nub, (\\), intersect, union, partition)

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Control.Monad
import Data.Monoid

import Forml.Types.Namespace hiding (Module)

type Id = String

enumId :: Int -> Id
enumId n = "v" ++ show n

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
    tv = nub . concat . map tv

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
    return x = Z x
    (Z x) >>= f = f x
    (Error x) >>= _ = Error x
    fail x = Error x

mgu :: Type -> Type -> TI Substitution
mgu x y = do z <- mgu' x y
             case z of
               Left e  -> add_error e >> return []
               Right e -> return e

mgu' :: Type -> Type -> TI (Either String Substitution)
mgu' x y = case x |=| y of
             Z z -> return $ Right z
             Error e -> second_chance e x y

    where second_chance e x@ (TypeRecord (TRecord _ (TPartial _) _)) y =
              
              do as <- get_assumptions
                 g  <- find''' as x

                 case g of
                   Nothing -> return $ Left e --add_error e >> return []
                   Just (x, sct) ->
                       do (qs' :=> t'') <- freshInst sct
                          return $ Right x

          second_chance e y x @ (TypeRecord (TRecord _ (TPartial _) _)) = second_chance e x y
          second_chance e (TypeApplication a b) (TypeApplication c d) =
              do xss <- second_chance e a c
                 yss <- second_chance e b d
                 case (xss, yss) of
                   (Right xss', Right yss') -> return$ Right$ xss' @@ yss'
                   (Left e', _) -> return $ Left e'
                   (_, Left e') -> return $ Left e'
                   

          second_chance e x y = case x |=| y of
                                  Error _ -> return $ Left e --add_error e >> return []
                                  Z x -> return $ Right x
 
          find''' [] _ = return Nothing
          find''' (_:>:_:xs) t = find''' xs t
          find''' ((Forall _ x :>>: y):xs) t =

              do (_ :=> t') <- return$ inst (map TypeVar$ tv t) x
                 case t' |=| t of
                   Error _ -> find''' xs t

                   -- TODO Only allow this shorthand if the match is unique - true?
                   Z x  -> do zz' <- find''' xs t
                              case zz' of
                                Nothing -> return$ Just (x, y)
                                Just _ -> return$ Nothing

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



-- Type Classes
-- --------------------------------------------------------------------------------

data Qual t = [Pred] :=> t deriving Eq
data Pred = IsIn Id Type deriving (Show, Eq)

instance (Show t) => Show (Qual t) where
    show (_ :=> t) = show t

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t)      = tv ps `union` tv t

instance Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn _ t)      = tv t

mguPred, matchPred :: Pred -> Pred -> Maybe Substitution
mguPred   = lift (|=|)
lift :: forall (m :: * -> *) a.
                       Monad m =>
                       (Type -> Type -> m a) -> Pred -> Pred -> m a
matchPred = lift match

lift m (IsIn i t) (IsIn i' t')
         | i == i'   = m t t'
         | otherwise = fail "classes differ"

type Class    = ([Id], [Inst])
type Inst     = Qual Pred



-- Class Environments
-- --------------------------------------------------------------------------------

data ClassEnv = ClassEnv { classes  :: Id -> Maybe Class,
                           defaults :: [Type] }

super :: ClassEnv -> Id -> [Id]
super ce i = case classes ce i of Just (is, _) -> is

insts :: ClassEnv -> Id -> [Inst]
insts ce i = case classes ce i of Just (_, its) -> its

defined :: Maybe a -> Bool
defined (Just x) = True
defined Nothing  = False

modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce{classes = \j -> if i==j then Just c
                                           else classes ce j}

initialEnv :: ClassEnv
initialEnv  = ClassEnv { classes  = \_ -> fail "class not defined",
                         defaults = [num_type] }

type EnvTransformer = ClassEnv -> Maybe ClassEnv

infixr 5 <::>
(<::>)       :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <::> g) ce = do ce' <- f ce
                   g ce'

addClass                              :: Id -> [Id] -> EnvTransformer
addClass i is ce
 | defined (classes ce i)              = fail "class already defined"
 | any (not . defined . classes ce) is = fail "superclass not defined"
 | otherwise                           = return (modify ce i (is, []))

addPreludeClasses :: EnvTransformer
addPreludeClasses  = addCoreClasses <::> addNumClasses

addCoreClasses ::   EnvTransformer
addCoreClasses  =   addClass "Eq" []
                <::> addClass "Ord" ["Eq"]
                <::> addClass "Show" []
                <::> addClass "Read" []
                <::> addClass "Bounded" []
                <::> addClass "Enum" []
                <::> addClass "Functor" []
                <::> addClass "Monad" []

addNumClasses  ::   EnvTransformer
addNumClasses   =   addClass "Num" ["Eq", "Show"]
                <::> addClass "Real" ["Num", "Ord"]
                <::> addClass "Fractional" ["Num"]
                <::> addClass "Integral" ["Real", "Enum"]
                <::> addClass "RealFrac" ["Real", "Fractional"]
                <::> addClass "Floating" ["Fractional"]
                <::> addClass "RealFloat" ["RealFrac", "Floating"]



-- Entailment
-- --------------------------------------------------------------------------------

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) =
    p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t)    = msum [ tryInst it | it <- insts ce i ]
 where tryInst (ps :=> h) = do u <- matchPred h p
                               Just (map (apply u) ps)

entail        :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
                 case byInst ce p of
                   Nothing -> False
                   Just qs -> all (entail ce ps) qs

inHnf       :: Pred -> Bool
inHnf (IsIn _ t) = hnf t
   where hnf (TypeVar v)  = True
         hnf (Type tc) = False
         hnf (TypeApplication t _) = hnf t

toHnfs      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)

toHnf                 :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p | inHnf p   = return [p]
           | otherwise = case byInst ce p of
                           Nothing -> fail "context reduction"
                           Just ps -> toHnfs ce ps

simplify   :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
 where loop rs []                            = rs
       loop rs (p:ps) | entail ce (rs++ps) p = loop rs ps
                      | otherwise            = loop (p:rs) ps

reduce      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do qs <- toHnfs ce ps
                  return (simplify ce qs)

scEntail        :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)



-- Type Schemes
-- --------------------------------------------------------------------------------

data Scheme = Forall [Kind] (Qual Type) deriving Eq

instance Show Scheme where
    show (Forall [] t) = show t
    show (Forall xs t) = show t --"∀" ++ vars ++ " ⇒ " ++ show t
        where vars = concat . L.intersperse " " . map (\x -> (map (:[]) "abcdefghijklmnopqrstuvwxyz") !! x) . take (length xs) $ [0..]

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall _ qt)      = tv qt

quantify      :: [TypeVar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
 where vs' = [ v | v <- tv qt, v `elem` vs ]
       ks  = map kind vs'
       s   = zip vs' (map TypeGen [0..])

toScheme      :: Type -> Scheme
toScheme t     = Forall [] ([] :=> t)



-- Assumptions
-- --------------------------------------------------------------------------------

data Assumption = Id :>: Scheme | Scheme :>>: Scheme deriving (Eq)

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
              find' ((i':>>:sc):as) = do (_ :=> i'') <- freshInst i
                                         (_ :=> i''') <- freshInst i'
                                         x <- i'' `can_unify` i'''
                                         if x then return $ Just (i', sc) else find' as
              find' (_:as)          = find' as



-- Type Inference Monad
-- --------------------------------------------------------------------------------

data TIState = TIState { substitution :: Substitution
                       , seed :: Int
                       , class_env :: ClassEnv
                       , msg :: String
                       , warnings :: [String]
                       , errors :: [String]
                       , modules :: [(Namespace, [Assumption])]
                       , namespace :: Namespace
                       , assumptions :: [Assumption]
                       , predicates :: [Pred] }

newtype TI a = TI (TIState -> (TIState, a))

instance Monad TI where
  fail x     = TI (\y -> error$ x ++ "\n" ++ msg y) 
  return x   = TI (\y -> (y, x))
  TI f >>= g = TI (\x -> case f x of
                          (y, x) -> let TI gx = g x
                                   in  gx y)

instance Functor TI where
    fmap f y = y >>= (\x -> TI (\x' -> (x', f x)))

instance Types [(Namespace, [Assumption])] where
    apply s v = map (\(x, y) -> (x, apply s y)) v
    tv = concat . map (\(_, y) -> tv y)


runTI :: TI a -> a
runTI (TI f) = x where (t,x) = f (TIState [] 0 initialEnv "" [] [] [] (Namespace []) [] [])

get_assumptions  :: TI [Assumption]
get_msg          :: TI String
set_msg          :: String -> TI ()
get_predicates   :: TI [Pred]
set_predicates   :: [Pred] -> TI ()
get_substitution :: TI Substitution
get_classenv     :: TI ClassEnv
add_error        :: String -> TI ()

get_assumptions  = TI (\x -> (x, assumptions x))
get_msg          = TI (\x -> (x, msg x))
set_msg x        = TI (\y -> (y { msg = x }, ()))
get_predicates   = TI (\x -> (x, predicates x))
set_predicates x = TI (\y -> (y { predicates = x }, ()))
get_substitution = TI (\x -> (x, substitution x))
get_classenv     = TI (\x -> (x, class_env x))
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

class Predicated a where
    predicate :: a -> TI ()

instance Predicated Pred where
    predicate x = TI (\y -> (y { predicates = predicates y ++ (x:[]) }, ()))

instance Predicated [Pred] where
    predicate x = TI (\y -> (y { predicates = predicates y ++ x}, ()))


with_scope :: TI a -> TI a
with_scope x = do as <- get_assumptions
                  ps <- get_predicates
                  y <- x
                  set_assumptions as
                  set_predicates ps
                  return y

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

freshInst :: Scheme -> TI (Qual Type)
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

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

class Infer e t | e -> t where infer :: e -> TI t


list_scheme :: Scheme
list_scheme = Forall [Star] qual_list
    where qual_list = [] :=> TypeApplication (Type (TypeConst "List" (KindFunction Star Star))) (TypeGen 0)

bool_type :: Type
bool_type = Type (TypeConst "Bool" Star)

-- Expressions


infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TypeApplication (TypeApplication fun_type a) b
