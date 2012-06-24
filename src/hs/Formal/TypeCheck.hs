
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
{-# LANGUAGE UndecidableInstances #-}

module Formal.TypeCheck where
import System.IO.Unsafe

import List (nub, (\\), intersect, union, partition)
import Monad (msum)

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Data.Monoid
import Data.Char

import Text.ParserCombinators.Parsec

import Formal.Types.Literal
import Formal.Types.Pattern
import Formal.Types.Symbol
import Formal.Types.Expression
import Formal.Types.Definition
import Formal.Types.Axiom
import Formal.Types.Statement hiding (find, namespace, modules, Test)
import Formal.Types.Namespace hiding (Module)

import Formal.Types.TypeDefinition
import Formal.Types.Type

import Formal.Parser.Utils
import Formal.Parser

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
    show (TypeRecord (TRecord m (TPartial z) _)) =
        "{" ++ (concat$ L.intersperse ", "$ map (\(x, y) -> x ++ " = " ++ show y) . M.toList $ m) ++ ", _ }"


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

        | otherwise = fail$ "Records do not unify: found " ++ show (TypeRecord t')
                             ++ ", expecting " ++ show (TypeRecord u')
                    
    t |=| u @ (TRecord _ (TPartial _) _) = u |=| t

    t |=| u = fail$ "Unimplemented record unification " ++ show (TypeRecord t) ++ ", " ++ show (TypeRecord u)

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
mgu x y = case x |=| y of
             Z z -> return z
             Error e -> second_chance e x y

    where second_chance e x@ (TypeRecord (TRecord _ (TPartial _) _)) y =
              
              do as <- get_assumptions
                 g  <- find''' as x

                 case g of
                   Nothing -> add_error e >> return []
                   Just (x, sct) ->
                       do (qs' :=> t'') <- freshInst sct
                          return x

          second_chance e y x @ (TypeRecord (TRecord _ (TPartial _) _)) = second_chance e x y
          second_chance e (TypeApplication a b) (TypeApplication c d) =
              do xss <- second_chance e a c
                 yss <- second_chance e b d
                 return$ xss @@ yss

          second_chance e x y = case x |=| y of
                                  Error _ -> add_error e >> return []
                                  Z x -> return x
 
          find''' [] _ = return Nothing
          find''' (_:>:_:xs) t = find''' xs t
          find''' (_:>>:(Forall _ x, y):xs) t =

              do (_ :=> t') <- return$ inst (map TypeVar$ tv t) x
                 case t |=| t' of
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
  tv (IsIn i t)      = tv t

mguPred, matchPred :: Pred -> Pred -> Maybe Substitution
mguPred   = lift (|=|)
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
super ce i = case classes ce i of Just (is, its) -> is

insts :: ClassEnv -> Id -> [Inst]
insts ce i = case classes ce i of Just (is, its) -> its

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
inHnf (IsIn c t) = hnf t
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

data RecordId = RecordId (M.Map String RecordId)
              | RecordPartial (M.Map String RecordId)
              | RecordKey


data Assumption = Id :>: Scheme | RecordId :>>: (Scheme, Scheme) deriving (Eq)

newtype A = A [Assumption]

instance Eq RecordId where
    (RecordId m) == (RecordId n) = m == n
    RecordKey == RecordKey = True
    (RecordPartial m) == (RecordId n) = m == ((M.\\) n ((M.\\) n  m))
    n == m @ (RecordPartial _) = m == n
    _ == _ = False

instance Show RecordId where
    show (RecordId m) = "{" ++ (concat$ L.intersperse ", "$ M.keys m) ++ "}"
    show (RecordPartial m) = "{" ++ (concat$ L.intersperse ", "$ M.keys m) ++ ", _}"

instance Show Assumption where
    show (i :>: s)  = i ++ ": " ++ show s
    show (i :>>: (_,s)) = show i ++ ": " ++ show s

instance Types Assumption where
    apply s (i :>: sc)  = i :>: (apply s sc)
    apply s (i :>>: (sc, sd)) = i :>>: (apply s sc, apply s sd)

    tv (_ :>: sc)       = tv sc
    tv (_ :>>: (sc, _)) = tv sc

class Find a b | a -> b where
    find :: a -> TI b

instance Find Id Scheme where

    find i = do (reverse -> x) <- get_assumptions
                find' x 

        where find' ((i':>:sc):as) = if i == i' then return sc else find' as
              find' (_:as)         = find' as
              find' []             = do add_error ("Unbound identifier " ++ show i)
                                        return$ toScheme$ TypeVar (TVar "a" Star)
              

subkey :: RecordId -> RecordId -> Bool
subkey (RecordId m) (RecordId n) = M.keys m == M.keys n && all id (zipWith subkey (M.elems m) (M.elems n))
subkey _ _ = True

instance Find RecordId (Maybe (Scheme, Scheme)) where

    find i = do (reverse -> x) <- get_assumptions
                return$ find' x 

        where find' []              = Nothing
              find' ((i':>>:sc):as) = if i `subkey` i' then Just sc else find' as
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
get_errors       = TI (\x -> (x, errors x))
get_modules      = TI (\x -> (x, modules x))
set_assumptions x = TI (\y -> (y { assumptions = x }, ()))
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

with_module :: String -> TI () -> TI ()
with_module name x = do as <- get_assumptions
                        ns <- get_namespace
                        set_namespace (ns `mappend` Namespace [name])
                        x
                        as' <- get_assumptions
                        set_assumptions as
                        add_module (ns `mappend` Namespace [name], drop (length as) as')
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



-- Type Inference
-- --------------------------------------------------------------------------------

class Infer e t | e -> t where infer :: e -> TI t

instance Infer Literal Type where
    infer (StringLiteral _) = return (Type (TypeConst "String" Star))
    infer (IntLiteral _)    = return (Type (TypeConst "Num" Star))
    infer (IntLiteral _)    = return (Type (TypeConst "Num" Star))

instance Infer (Pattern b) Type where
    infer (VarPattern i) = do v <- newTVar Star
                              assume (i :>: toScheme v)
                              return v

    infer AnyPattern = newTVar Star

    infer (LiteralPattern x) = infer x

    infer (ListPattern xs) = do ts <- mapM infer xs
                                t' <- newTVar Star
                                (qs :=> t) <- freshInst list_scheme
                                mapM_ (unify t') ts
                                predicate qs
                                return t

    infer m @ (RecordPattern (unzip . M.toList -> (names, patterns)) p) =
        
        do ts <- mapM infer patterns
           sc <- find$ key m
           p' <- case p of
                   Complete -> return TComplete
                   Partial  -> do t <-  newTVar Star
                                  return$ TPartial t

           let r = TypeRecord (TRecord (M.fromList (zip (map f names) ts)) p' Star)
           t' <- newTVar Star
           case sc of
             Nothing ->
                 do unify t' r
                    return t'
             Just (Forall _ scr, sct) ->
                 do (qs' :=> t'') <- freshInst sct
                    (qs :=> t) <- return$ inst (map TypeVar$ tv t'') scr
                    (qs :=> t) <- freshInst (quantify (tv t \\ tv t'') (qs :=> t))
                    unify t r
                    unify t' t''
                    s <- get_substitution
                    let t''' = apply s t
                        r''' = apply s r
                        qt = quantify (tv t''') $ [] :=> t'''
                        rt = quantify (tv r''') $ [] :=> r'''
                    if qt /= rt
                        then do add_error$ "Object constructor does not match signature\n" 
                                             ++ "  Expected: " ++ show qt ++ "\n" 
                                             ++ "  Actual:   " ++ show rt
                                return t'
                        else return t'

        where f (Symbol x) = x
              f (Operator x) = x

list_scheme :: Scheme
list_scheme = Forall [Star] qual_list
    where qual_list = [] :=> TypeApplication (Type (TypeConst "List" (KindFunction Star Star))) (TypeGen 0)

bool_type :: Type
bool_type = Type (TypeConst "Bool" Star)

-- Expressions


infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TypeApplication (TypeApplication fun_type a) b
 
instance Infer (Expression Definition) Type where
    infer (ApplyExpression _ []) = fail "This should not be"
    infer (ApplyExpression e (x:[])) =

        do te <- infer e
           tx <- infer x
           t  <- newTVar Star
           unify (tx `fn` t) te
           return t

    infer (ApplyExpression e (x:xs)) = infer (ApplyExpression (ApplyExpression e (x:[])) xs)

    infer (IfExpression a b c) =
        
        do ta <- infer a
           tb <- infer b
           tc <- infer c
           t  <- newTVar Star
           unify ta bool_type
           unify t tb
           unify t tc
           return t

    infer (LiteralExpression s) = infer s

    infer (SymbolExpression i) =

        do sc <- find (show i)
           (ps :=> t) <- freshInst sc
           predicate ps
           return t

    infer (JSExpression _) =

        do t <- newTVar Star
           return (TypeApplication (Type (TypeConst "JS" (KindFunction Star Star))) t)

    infer (LazyExpression x _) =

        do t <- newTVar Star
           t' <- infer x
           unify t t'
           return (TypeApplication (Type (TypeConst "JS" (KindFunction Star Star))) t)


    -- TODO this may be removeable at no perf cost?
    infer (FunctionExpression rs) =

        do t <- newTVar Star
           as <- get_assumptions
           [_ :>: q] <- with_scope$ ims as
           (_ :=> t') <- freshInst q
           unify t t'
           return t

        where ims as = 

                  do infer [Definition Public False (Symbol "") rs]
                     as'' <- get_assumptions
                     return$ as'' \\ as

    infer (AccessorExpression (Addr s f x) y) = infer (acc y)

        where acc :: [Symbol] -> Expression Definition
              acc [] = x
              acc (y:ys) = --Addr undefined undefined $
                  ApplyExpression 
                  (FunctionExpression 
                   [ EqualityAxiom 
                     (Match [RecordPattern (M.fromList [(y, VarPattern "__x__")]) Partial] Nothing)
                     (Addr s f (SymbolExpression (Symbol "__x__"))) ] )
                  [acc ys]
 
    infer m @ (RecordExpression (unzip . M.toList -> (names, xs))) =

        do ts <- mapM infer xs
           sc <- find$ key m
           let r = TypeRecord (TRecord (M.fromList (zip (map f names) ts)) TComplete Star)
           t' <- newTVar Star
           case sc of
             Nothing ->
                 do unify t' r
                    return t'
             Just (Forall _ scr, sct) ->
                 do (qs' :=> (t'')) <- freshInst sct
                    (qs :=> t) <- return$ inst (map TypeVar$ tv t'') (scr)
                    (qs :=> t) <- freshInst (quantify (tv t \\ tv t'') (qs :=> t))
                    unify t r
                    unify t' t''
                    s <- get_substitution
                    let t''' = apply s t
                        r''' = apply s r
                        qt   = quantify (tv t''') $ [] :=> t'''
                        rt   = quantify (tv r''') $ [] :=> r'''
                        sct' = apply s t''
                    if qt /= rt
                        then do add_error$ "Record does not match expected signature for " ++ show sct' ++ "\n" 
                                             ++ "  Expected: " ++ show qt ++ "\n" 
                                             ++ "  Actual:   " ++ show rt
                                return t'
                        else return t'

        where f (Symbol x) = x
              f (Operator x) = x
                                                   
    infer (LetExpression xs x) =

        with_scope$ do mapM infer defs 
                       infer x

        where defs = to_group (map DefinitionStatement xs)

    infer (ListExpression x) =

        do t <- newTVar Star
           ts <- mapM infer x
           mapM (unify t) ts
           t' <- newTVar Star
           unify t' (TypeApplication (Type (TypeConst "Array" (KindFunction Star Star))) t)
           return t'

-- Axioms

instance (Infer a t) => Infer (Addr a) t where
    
    infer (Addr s f x) = do m <- get_msg
                            set_msg new_msg
                            z <- infer x
                            set_msg m
                            return z

        where new_msg = "  at line " ++ show (sourceLine s) ++ ", column " ++ show (sourceColumn s) ++ "\n"

instance Infer (Axiom (Expression Definition)) Type where

    infer (EqualityAxiom (Match y z) x) =

        do ts <- mapM infer y
           case z of 
             (Just q) -> infer q >>= (flip unify) bool_type 
             _ -> return ()
           t  <- infer x
           return (foldr fn t ts)

    infer _ = newTVar Star

 



-- Generalization

split :: Monad m => ClassEnv -> [TypeVar] -> [TypeVar] -> [Pred] -> m ([Pred], [Pred])
split ce fs gs ps =

    do ps' <- reduce ce ps
       let (ds, rs) = partition (all (`elem` fs) . tv) ps'
       return (ds, rs) -- \\ rs')

instance Infer [Definition] () where
    infer bs =

        do def_types <- mapM (\_ -> newTVar Star) bs

           let is    = map get_name bs
               scs   = map toScheme def_types
               altss = map get_axioms bs
               
           axiom_types <- with_scope$ 
                do assume $ zipWith (:>:) is scs
                   mapM (mapM (with_scope . infer)) altss

           let f _ []     = return ()
               f g (x:xs) = do s <- get_substitution
                               g x
                               g (apply s x) 
                               f g xs

           mapM (\(t, as) -> f (unify t) as) (zip def_types axiom_types)
           
           ps  <- get_predicates
           as  <- get_assumptions
           ps' <- substitute ps
           ss  <- get_substitution
           fs' <- substitute as

           let ts' = apply ss def_types
               fs  = tv fs'
               vss = map tv ts'
               gs  = foldr1 union vss \\ fs

           ce <- get_classenv
           (ds, rs) <- split ce fs (foldr1 intersect vss) ps'

           if restricted then
               let gs'  = gs \\ tv rs
                   scs' = map (quantify gs' . ([]:=>)) ts'
               in do predicate (ds ++ rs)
                     assume (zipWith (:>:) is scs')
                     return ()
             else
               let scs' = map (quantify gs . (rs:=>)) ts'
               in do predicate ds
                     assume (zipWith (:>:) is scs')
                     return ()

        where get_name (Definition _ _ (Symbol x) _) = x
              get_name (Definition _ _ (Operator x) _) = x
              get_axioms (Definition _ _ _ x) = x

              restricted = any simple bs
              simple (Definition _ _ i axs) = any (null . f) axs

              f (EqualityAxiom (Match p _) _) = p
              f _ = error "Fatal error occurred while reticulating splines"


data BindGroup = Scope [Namespace] [Statement] [Definition] [[Definition]] [Addr (Expression Definition)]
               | Module String [BindGroup]
               deriving (Show)

instance Infer Definition () where

    infer (Definition _ _ name axs) =

        do sc <- find$ f name 
           (qs :=> t)  <- freshInst sc
           axiom_types <- with_scope$ mapM (with_scope . infer) axs

           s <- get_substitution
           mapM (unify t) (apply s axiom_types)

           as <- get_assumptions
           ce <- get_classenv
           ps <- get_predicates
           s  <- get_substitution

           let qs' = apply s qs
               t'  = apply s t
               fs  = tv (apply s as)
               gs  = tv t' \\ fs
               sc' = quantify gs (qs' :=> t')
               ps' = filter (not . entail ce qs') (apply s ps)

           (ds, rs) <- split ce fs gs ps'

           if sc /= sc' then
               add_error$ "Signature too general\n\n    Expected: " ++ show sc ++ "\n    Actual: " ++ show sc'
             else if not (null rs) then
               add_error$ "Context too weak\n\n    Expected: " ++ show sc ++ "\n    Actual: " ++ show sc'
             else
               assume (f name :>: sc)

           return ()

        where f (Symbol x) = x
              f (Operator x) = x

instance Infer Test () where

    infer (Test ex) = do t <- newTVar Star
                         x <- infer ex
                         unify t x
                         unify t bool_type
               
newtype Test = Test (Addr (Expression Definition))

instance Infer BindGroup () where
    infer (Scope imps tts es iss ts) =

        do as <- get_assumptions
           mapM import' imps
           as' <- get_assumptions
           infer tts
           mapM assume$ sigs es
           mapM infer iss
           with_scope$ mapM infer es
           mapM infer (map Test ts)
           as'' <- get_assumptions
           set_assumptions$ as'' \\ (as' \\ as)
           return ()

        where f (TypeAxiom t) = True
              f _ = False

              g name (TypeAxiom t) = [ name :>: to_scheme' t' | t' <- enumerate_types t ]

              to_scheme' :: Type -> Scheme
              to_scheme' t = quantify (tv t) ([] :=> t)

              sigs :: [Definition] -> [Assumption]
              sigs [] = []
              sigs (Definition _ _ name as:xs) =
                  case L.find f as of
                    Nothing -> sigs xs
                    Just x -> g (h name) x ++ sigs xs

              import' (Namespace ns) =

                  do z <- get_modules
                     a <- get_assumptions
                     (Namespace ns') <- get_namespace
                     case Namespace ns `lookup` z of
                       Just z' -> assume$ a ++ z'
                       Nothing -> if length ns' > 0 && head ns' /= head ns
                                  then import' (Namespace (head ns' : ns))
                                  else add_error$ "Unknown namespace " ++ show (Namespace ns)
              
              h (Symbol x) = x
              h (Operator x) = x

    infer (Module name bgs) = 

        with_module name$ do mapM infer bgs
                             return ()
    

class ToKey a where
    key :: a -> RecordId

instance ToKey (Pattern a) where
    key (RecordPattern m Complete) = RecordId . M.mapKeys show . M.map (\x -> key x) $ m
    key (RecordPattern m Partial)  = RecordId . M.mapKeys show . M.map (\x -> key x) $ m
    key _ = RecordKey

instance ToKey (Expression a) where
    key (RecordExpression m) = RecordId . M.mapKeys show . M.map (\x -> key x) $ m
    key _ = RecordKey

instance ToKey Type where
    key (TypeRecord (TRecord m TComplete _)) = RecordId . M.map (\x -> key x) $ m
    key (TypeRecord (TRecord m (TPartial _) _)) = error "Unimplemented" --RecordId . M.map (\x -> key x) $ m
    key _ = RecordKey
    

to_scheme :: TypeDefinition -> UnionType -> [Assumption]
to_scheme (TypeDefinition n vs) t = [ key y :>>: (quantify (vars y) ([]:=> y), def_type y)
                                          | y <- enumerate_types t ] 

    where vars y = map (\x -> TVar x (infer_kind x y)) vs

          def_type y = quantify (vars y) ([] :=> foldl app poly_type (map TypeVar (vars y)))

          poly_type = Type (TypeConst n (to_kind (length vs)))

          to_kind 0 = Star
          to_kind n = KindFunction Star (to_kind$ n - 1)

          app :: Type -> Type -> Type
          app y x = TypeApplication y x

          -- TODO this is still wrong - have to check for all enumerated types

          infer_kind x y = let ks = infer_kinds x y
                           in if ks == []
                              then Star 
                              else if all (\x -> x == head ks) ks
                                   then head ks
                                   else error "Kind mismatch in schemer"

          infer_kinds x (TypeApplication a b) = infer_kinds x a ++ infer_kinds x b
          infer_kinds x (TypeVar (TVar y k)) | x == y = [k]
          infer_kinds x (TypeRecord (TRecord m _ _)) = concat$ map (infer_kinds x) (M.elems m)
          infer_kinds _ _ = []

-- | Computes all possible types from a type signature AST.

enumerate_types :: UnionType -> [Type]
enumerate_types (UnionType types) = concat . map enumerate_type . S.toList $ types

    where term_type (VariableType x)      = [ TypeVar (TVar x Star) ]
          term_type (SymbolType x)        = [ Type (TypeConst (show x) Star) ]
          term_type (PolymorphicType a b) = [ foldl TypeApplication a' b' 
                                                  | b' <- map enumerate_types b
                                                  , a' <- to_kind' (length b')$ term_type a ]

          to_kind 0 = Star
          to_kind n = KindFunction Star (to_kind$ n - 1)

          to_kind' _ [] = []
          to_kind' n (TypeVar (TVar x _) : xs) = TypeVar (TVar x (to_kind n)) : to_kind' n xs
          to_kind' n (Type (TypeConst x _) : xs) = Type (TypeConst x (to_kind n)) : to_kind' n xs

          enumerate_type (SimpleType x) = term_type x

          enumerate_type (FunctionType a b) =
              [ a' `fn` b' | a' <- enumerate_types a, b' <- enumerate_types b ]

          enumerate_type (RecordType (unzip . M.toList -> (names, types'))) =

              map f permutations
        
              where f = TypeRecord . (\x -> TRecord x TComplete Star) . M.fromList . zip (map show names)
                    permutations = permutations' . map enumerate_types $ types'

                        where permutations' [] = [] 
                              permutations' (x:[]) = [ x ]
                              permutations' (x:xs) = [ x' : xs' | x' <- x, xs' <- permutations' xs ]



db :: Show a => a -> a
db x = unsafePerformIO $ do putStrLn$ "-- " ++ (show x)
                            return x

instance Infer [Statement] () where

    infer [] = return ()
    infer (TypeStatement t c : xs) = 

        do assume     $ to_scheme t c
           infer xs

    infer (_ : xs) = infer xs

sort_dep :: [[Definition]] -> [[Definition]]
sort_dep [] = []
sort_dep xs = case map (:[])$ concat$ map snd$ filter fst free of
                [] -> error$ "Unresolvable dependency ordering in " ++ show (get_names xs)
                xs -> xs ++ sort_dep (map snd$ filter (not . fst) free)


    where as = get_needed (get_names xs) (zip (get_names xs) (get_expressions xs))

          free = get_free as `zip` xs

          get_free [] = []
          get_free ([]:xs) = True : get_free xs
          get_free (_:xs) = False : get_free xs 

          get_names [] = []
          get_names ([Definition _ _ n _]:xs) = show n : get_names xs

          get_needed _ [] = []
          get_needed names ((n,x):xs) =
              ((names \\ [n]) `L.intersect` (concat$ map get_symbols x)) : get_needed names xs

          get_expressions :: [[Definition]] -> [[Expression Definition]]
          get_expressions [] = []
          get_expressions ([Definition _ _ _ as]:xs) = get_expressions' as : get_expressions xs

          get_expressions' [] = []
          get_expressions' (TypeAxiom _: xs) = get_expressions' xs
          get_expressions' (EqualityAxiom (Match _ (Just y)) (Addr _ _ x): xs) = y : x : get_expressions' xs
          get_expressions' (EqualityAxiom _ (Addr _ _ x): xs) = x : get_expressions' xs

          get_symbols (RecordExpression (unzip . M.toList -> (_, xs))) = concat (map get_symbols xs)
          get_symbols (AccessorExpression (Addr _ _ x) _) = get_symbols x
          get_symbols (ApplyExpression a b)   = get_symbols a ++ concat (map get_symbols b)
          get_symbols (IfExpression a b c)    = get_symbols a ++ get_symbols b ++ get_symbols c
          get_symbols (LiteralExpression _)   = []
          get_symbols (SymbolExpression x)    = [show x]
          get_symbols (JSExpression _)        = []
          get_symbols (LazyExpression (Addr _ _ x) _)      = get_symbols x
          get_symbols (FunctionExpression as) = concat$ map get_symbols$ get_expressions' as
          get_symbols (LetExpression _ x)     = get_symbols x
          get_symbols (ListExpression x)      = concat (map get_symbols x)

js_type = Type (TypeConst "JS" (KindFunction Star Star))

tiProgram :: Program -> [(Namespace, [Assumption])] -> ([(Namespace, [Assumption])], [String])
tiProgram (Program bgs) env = 
    
    runTI $ do TI (\x -> (x { modules = env }, ()))
               assume$ "true"  :>: (Forall [] ([] :=> Type (TypeConst "Bool" Star)))
               assume$ "false" :>: (Forall [] ([] :=> Type (TypeConst "Bool" Star)))
               assume$ "error" :>: (Forall [Star] ([] :=> TypeGen 0))
               assume$ "run"   :>: (Forall [Star] ([] :=> (TypeApplication js_type (TypeGen 0) -:> TypeGen 0)))
               mapM infer$ to_group bgs
               s  <- get_substitution
               ce <- get_classenv
               ps <- get_predicates
               ms <- TI (\y -> (y, modules y))
               rs <- reduce ce (apply s ps)
               e  <- get_errors
               return ((apply s ms), S.toList . S.fromList $ e)

to_group :: [Statement] -> [BindGroup]
to_group [] = []
to_group xs = case takeWhile not_module xs of
                [] -> to_group' xs
                yx -> sort_deps (foldl f (Scope [] [] [] [] []) yx) 
                      : to_group' (dropWhile not_module xs)

    where to_group' [] = []
          to_group' (ModuleStatement x y:xs) = Module (show x) (to_group y) : to_group xs
          to_group' _ = error "Unexpected"

          sort_deps (Scope i t a b c) = Scope i t a (sort_dep b) c

          not_module (ModuleStatement _ _) = False
          not_module _ = True

          f (Scope i t a b c) (DefinitionStatement x @ (Definition _ _ _ (EqualityAxiom _ _:_))) =
              Scope i t a (b ++ [[x]]) c
          f (Scope i t a b c) (DefinitionStatement x @ (Definition _ _ _ (TypeAxiom _:_))) =
              Scope i t (a ++ [x]) b c
          f (Scope i t a b c) (ExpressionStatement x) =
              Scope i t a b (c ++ [x])
          f (Scope i t a b c) (ImportStatement ns) =
              Scope (i ++ [ns]) t a b c
          f (Scope i t a b c) x @ (TypeStatement _ _) =
              Scope i (t ++ [x]) a b c
          f x _ = x

