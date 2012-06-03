
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

import Formal.Types.Literal
import Formal.Types.Pattern
import Formal.Types.Symbol
import Formal.Types.Expression
import Formal.Types.Definition
import Formal.Types.Axiom
import Formal.Types.Statement hiding (find)

import Formal.Types.TypeDefinition
import Formal.Types.Type

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
    show (TypeRecord (TRecord m _)) = show m


data TypeRecord = TRecord (M.Map Key Type) Kind
                deriving (Show, Eq)

data TypeVar = TVar Id Kind
               deriving (Show, Eq, Ord)

data TypeConst = TypeConst Id Kind -- (?)
                 deriving (Show, Eq, Ord)

num_type  = Type (TypeConst "Num" Star)
fun_type  = Type (TypeConst "->"  (KindFunction Star (KindFunction Star Star)))

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
    kind t = Star -- error$ "Could not determine kind of " ++ show t

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
    apply s (TypeRecord (TRecord xs k)) = TypeRecord (TRecord (fmap (apply s) xs) k)
    apply _ t = t

    tv (TypeVar u) = [u]
    tv (TypeApplication l r) = tv l `union` tv r
    tv (TypeRecord (TRecord xs _)) = nub $ M.elems xs >>= tv
    tv t = []

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

mgu      :: Monad m => Type -> Type -> m Substitution
var_bind :: Monad m => TypeVar -> Type -> m Substitution
match    :: Monad m => Type -> Type -> m Substitution


mgu (TypeApplication l1 r1) (TypeApplication l2 r2) =
    do s1 <- mgu l1 l2
       s2 <- mgu (apply s1 r1) (apply s1 r2)
       return $ s1 @@ s2

mgu (TypeVar u) t  = var_bind u t
mgu t (TypeVar u)  = var_bind u t

mgu (TypeRecord (TRecord t _)) (TypeRecord (TRecord u _)) 
    | M.keys t `intersect` M.keys u == M.keys t = 

        f (M.elems t) (M.elems u) []

    where f [] _ s = return s
          f (x:xs) (y:ys) s = do s' <- mgu (apply s x) (apply s y)
                                 f xs ys (s @@ s')

mgu (Type t) (Type u) | t == u = return []
mgu t u = fail $ "Types do not unify: " ++ show t ++ " and " ++ show u

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
    | t == u           = return [] -- (?)  is this how records should be matched?
match (Type t) (Type u)
    | t == u           = return []
match t u             = fail "Types do not match"



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
mguPred   = lift mgu
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

infixr 5 <:>
(<:>)       :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do ce' <- f ce
                  g ce'

addClass                              :: Id -> [Id] -> EnvTransformer
addClass i is ce
 | defined (classes ce i)              = fail "class already defined"
 | any (not . defined . classes ce) is = fail "superclass not defined"
 | otherwise                           = return (modify ce i (is, []))

addPreludeClasses :: EnvTransformer
addPreludeClasses  = addCoreClasses <:> addNumClasses

addCoreClasses ::   EnvTransformer
addCoreClasses  =   addClass "Eq" []
                <:> addClass "Ord" ["Eq"]
                <:> addClass "Show" []
                <:> addClass "Read" []
                <:> addClass "Bounded" []
                <:> addClass "Enum" []
                <:> addClass "Functor" []
                <:> addClass "Monad" []

addNumClasses  ::   EnvTransformer
addNumClasses   =   addClass "Num" ["Eq", "Show"]
                <:> addClass "Real" ["Num", "Ord"]
                <:> addClass "Fractional" ["Num"]
                <:> addClass "Integral" ["Real", "Enum"]
                <:> addClass "RealFrac" ["Real", "Fractional"]
                <:> addClass "Floating" ["Fractional"]
                <:> addClass "RealFloat" ["RealFrac", "Floating"]



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
              | RecordKey
                deriving (Ord, Eq)

data Assumption = Id :>: Scheme | RecordId :>>: (Scheme, Scheme) deriving (Eq, Show)

newtype A = A [Assumption]

instance Show RecordId where
    show (RecordId m) = "{" ++ (concat$ L.intersperse ", "$ M.keys m) ++ "}"

instance Show A where
    show (A []) = ""
    show (A (i :>: s : xs))  = i ++ ": " ++ show s ++ "\n" ++ show (A xs)
    show (A (i :>>: (_,s) : xs)) = show i ++ ": " ++ show s ++ "\n" ++ show (A xs)


instance Types Assumption where
    apply s (i :>: sc)  = i :>: (apply s sc)
    apply s (i :>>: (sc, sd)) = i :>>: (apply s sc, apply s sd)

    tv (i :>: sc)       = tv sc
    tv (i :>>: (sc, _)) = tv sc

class Find a b | a -> b where
    find :: a -> TI b

instance Find Id Scheme where

    find i = do (reverse -> x) <- get_assumptions
                find' x 

        where find' []             = fail ("unbound identifier: " ++ show i)
              find' ((i':>:sc):as) = if i == i' then return sc else find' as
              find' (_:as)         = find' as

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
                       , assumptions :: [Assumption]
                       , predicates :: [Pred] }

newtype TI a = TI (TIState -> (TIState, a))

instance Monad TI where
  return x   = TI (\y -> (y, x))
  TI f >>= g = TI (\x -> case f x of
                          (y, x) -> let TI gx = g x
                                   in  gx y)

runTI :: TI a -> a
runTI (TI f) = x where (t,x) = f (TIState [] 0 initialEnv [] [])

get_substitution :: TI Substitution
get_substitution = TI (\x -> (x, substitution x))

get_classenv :: TI ClassEnv
get_classenv = TI (\x -> (x, class_env x))

get_assumptions :: TI [Assumption]

class Assume a where
    assume :: a -> TI ()

instance Assume Assumption where
    assume x = TI (\y -> (y { assumptions = assumptions y ++ (x:[]) }, ()))

instance Assume [Assumption] where
    assume x = TI (\y -> (y { assumptions = assumptions y ++ x}, ()))

get_assumptions = TI (\x -> (x, assumptions x))
set_assumptions x = TI (\y -> (y { assumptions = x }, ()))



get_predicates :: TI [Pred]
get_predicates = TI (\x -> (x, predicates x))
set_predicates x = TI (\y -> (y { predicates = x }, ()))


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

substitute :: Types a => a -> TI a
substitute x = do y <- get_substitution
                  return $ apply y x

unify :: Type -> Type -> TI ()
unify t1 t2 = do s <- get_substitution
                 u <- mgu (apply s t1) (apply s t2)
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
  inst ts (TypeRecord (TRecord m k))  = TypeRecord (TRecord (M.map (inst ts) m) k)
  inst ts t   = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)



-- Type Inference
-- --------------------------------------------------------------------------------

class Infer e where infer :: e -> TI Type

instance Infer Literal where
    infer (StringLiteral _) = return (Type (TypeConst "String" Star))
    infer (IntLiteral _)    = return (Type (TypeConst "Num" Star))
    infer (IntLiteral _)    = return (Type (TypeConst "Num" Star))

instance ToKey (Pattern a) where
    key (RecordPattern m) = RecordId . M.mapKeys show . M.map (\x -> key x) $ m
    key _ = RecordKey

instance Infer (Pattern b) where
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

    infer m @ (RecordPattern (unzip . M.toList -> (names, patterns))) =
        
        do ts <- mapM infer patterns
           sc <- find$ key m
           let r = TypeRecord (TRecord (M.fromList (zip (map f names) ts)) Star)
           t' <- newTVar Star
           case sc of
             Nothing ->
                 do unify t' r
                    return t'
             Just (Forall _ scr, sct) ->
                 do (qs' :=> t'') <- freshInst sct
                    (qs :=> t) <- return$ inst (map TypeVar$ tv t'') scr
                    unify t r
                    unify t' t''
                    return t'

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
 
instance ToKey (Expression a) where
    key (RecordExpression m) = RecordId . M.mapKeys show . M.map (\x -> key x) $ m
    key _ = RecordKey

instance Infer (Expression Definition) where
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

    infer (JSExpression _) = newTVar Star

    -- TODO this may be removeable at no perf cost?
    infer (FunctionExpression rs) = do t <- newTVar Star
                                       as <- get_assumptions
                                       [_ :>: q] <- with_scope$ do tiImpls [Definition (Symbol "") rs]
                                                                   as'' <- get_assumptions
                                                                   return$ as'' \\ as
                                       (_ :=> t') <- freshInst q
                                       unify t t'
                                       return t

    infer m @ (RecordExpression (unzip . M.toList -> (names, xs))) =

        do ts <- mapM infer xs
           sc <- find$ key m
           let r = TypeRecord (TRecord (M.fromList (zip (map f names) ts)) Star)
           t' <- newTVar Star
           case sc of
             Nothing ->  do unify t' r
                            return t'
             Just (Forall _ scr, sct) ->
                 do (qs' :=> t'') <- freshInst sct
                    (qs :=> t) <- return$ inst (map TypeVar$ tv t'') scr
                    unify t r
                    unify t' t''
                    return t'

        where f (Symbol x) = x
              f (Operator x) = x
                                                   
    infer (LetExpression xs x) = with_scope$ do tiBindGroup$ Scope [] [] (map (:[]) xs) []
                                                infer x

    infer (ListExpression x) =

        do t <- newTVar Star
           ts <- mapM infer x
           mapM (unify t) ts
           t' <- newTVar Star
           unify t' (TypeApplication (Type (TypeConst "Array" (KindFunction Star Star))) t)
           return t'

-- Axioms

instance Infer (Axiom (Expression Definition)) where

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

log_state :: TI ()
log_state = do (db -> s) <- get_substitution
               (db -> a) <- get_assumptions
               return ()
               assume a

tiImpls :: [Definition] -> TI ()
tiImpls bs =

    do def_types <- mapM (\_ -> newTVar Star) bs
       let is    = map get_name bs
           scs   = map toScheme def_types
           altss = map get_axioms bs
           
       axiom_types <- with_scope $ 
            do assume $ zipWith (:>:) is scs
               mapM (with_scope . mapM infer) altss

       s <- get_substitution
       let axiom_types' = apply s axiom_types
       mapM (\(t, as) -> mapM (unify t) as) (zip def_types axiom_types')
       
       ps  <- get_predicates
       as  <- get_assumptions
       ps' <- substitute ps

       ss <- get_substitution
       fs' <- substitute as
       let ts' = apply ss def_types

       let fs  = tv fs'
           vss = map tv ts'
           gs  = foldr1 union vss \\ fs

       ce <- get_classenv
       (ds,rs) <- split ce fs (foldr1 intersect vss) ps'

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

    where get_name (Definition (Symbol x) _) = x
          get_name (Definition (Operator x) _) = x
          get_axioms (Definition _ x) = x

          restricted = any simple bs
          simple (Definition i axs) = any (null . f) axs

          f (EqualityAxiom (Match p _) _) = p
          f _ = error "Not Defined"


data BindGroup = Scope [Statement] [Definition] [[Definition]] [Expression Definition]
               | Module String [BindGroup]

tiExpl :: Definition -> TI ()
tiExpl (Definition name axs) =

    do sc <- find$ f name 
       (qs :=> t) <- freshInst sc
       axiom_types <- with_scope$ mapM (with_scope . infer) axs
       s <- get_substitution
       mapM (unify t) (apply s axiom_types)
       as <- get_assumptions
       ce <- get_classenv
       ps <- get_predicates
       let qs' = apply s qs
           t' = apply s t
           fs = tv (apply s as)
           gs = tv t' \\ fs
           sc' = quantify gs (qs' :=> t')
           ps' = filter (not . entail ce qs') (apply s ps)
       (ds, rs) <- split ce fs gs ps'
       if sc /= sc' then
           fail "signature too general"
         else if not (null rs) then
           fail "context too weak"
         else
           assume (f name :>: sc)

       return ()

    where f (Symbol x) = x
          f (Operator x) = x

tiTest :: Expression Definition -> TI ()
tiTest ex = do t <- newTVar Star
               x <- infer ex
               unify t x
               unify t bool_type
               

tiBindGroup :: BindGroup -> TI ()
tiBindGroup (Scope tts es iss ts) =

    do tiTypeDefs tts
       mapM assume$ sigs es
       mapM tiImpls iss
       with_scope$ mapM tiExpl es
       mapM tiTest ts
       return ()

    where f (TypeAxiom t) = True
          f _ = False

          g name (TypeAxiom t) = [ name :>: to_scheme' t' | t' <- enumerate_types t ]

          to_scheme' :: Type -> Scheme
          to_scheme' t = quantify (tv t) ([] :=> t)

          sigs :: [Definition] -> [Assumption]
          sigs [] = []
          sigs (Definition name as:xs) =
              case L.find f as of
                Nothing -> sigs xs
                Just x -> g (h name) x ++ sigs xs

          h (Symbol x) = x
          h (Operator x) = x

tiBindGroup (Module name bgs) = do with_scope$ mapM tiBindGroup bgs
                                   return ()
    

class ToKey a where
    key :: a -> RecordId

instance ToKey Type where
    key (TypeRecord (TRecord m _)) = RecordId . M.map (\x -> key x) $ m
    key _ = RecordKey
    

to_scheme :: TypeDefinition -> UnionType -> [Assumption]
to_scheme (TypeDefinition n vs) t = [ key y :>>: (quantify vars ([]:=> y), def_type) | y <- enumerate_types t ] 

    where vars :: [TypeVar]
          vars = map (flip TVar Star) vs

          def_type :: Scheme
          def_type = quantify vars ([] :=> foldl app (Type (TypeConst n Star)) (map TypeVar vars))

          app :: Type -> Type -> Type
          app y x = TypeApplication y x

-- | Computes all possible types from a type signature AST.

enumerate_types :: UnionType -> [Type]
enumerate_types (UnionType types) = concat . map enumerate_type . S.toList $ types

    where term_type (VariableType x)      = [ TypeVar (TVar x Star) ]
          term_type (SymbolType x)        = [ Type (TypeConst (show x) Star) ]
          term_type (PolymorphicType a b) = [ foldl TypeApplication a' b' 
                                                  | b' <- map enumerate_types b
                                                  , a' <- term_type a ]

          enumerate_type (SimpleType x) = term_type x

          enumerate_type (FunctionType a b) =
              [ a' `fn` b' | a' <- enumerate_types a, b' <- enumerate_types b ]

          enumerate_type (RecordType (unzip . M.toList -> (names, types'))) =

              map f permutations
        
              where f = TypeRecord . flip TRecord Star . M.fromList . zip (map show names)
                    permutations = permutations' . map enumerate_types $ types'

                        where permutations' (x:[]) = [ x ]
                              permutations' (x:xs) = [ x' : xs' | x' <- x, xs' <- permutations' xs ]



db :: Show a => a -> a
db x = unsafePerformIO $ do putStrLn$ "-- " ++ (show x)
                            return x

tiTypeDefs :: [Statement] -> TI ()
tiTypeDefs [] = return ()
tiTypeDefs (TypeStatement t c : xs) = 

     do assume     $ to_scheme t c
        tiTypeDefs xs

tiTypeDefs (_ : xs) = tiTypeDefs xs

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
          get_names ([Definition n _]:xs) = show n : get_names xs

          get_needed _ [] = []
          get_needed names ((n,x):xs) = ((names \\ [n]) `L.intersect` (concat$ map get_symbols x)) : get_needed names xs

          get_expressions :: [[Definition]] -> [[Expression Definition]]
          get_expressions [] = []
          get_expressions ([Definition _ as]:xs) = get_expressions' as : get_expressions xs

          get_expressions' [] = []
          get_expressions' (TypeAxiom _: xs) = get_expressions' xs
          get_expressions' (EqualityAxiom (Match _ (Just y)) x: xs) = y : x : get_expressions' xs
          get_expressions' (EqualityAxiom _ x: xs) = x : get_expressions' xs

          get_symbols (ApplyExpression a b) = get_symbols a ++ concat (map get_symbols b)
          get_symbols (IfExpression a b c) = get_symbols a ++ get_symbols b ++ get_symbols c
          get_symbols (LiteralExpression _) = []
          get_symbols (SymbolExpression x) = [show x]
          get_symbols (JSExpression _) = []
          get_symbols (FunctionExpression as) = concat$ map get_symbols$ get_expressions' as
          get_symbols (RecordExpression (unzip . M.toList -> (_, xs))) = concat (map get_symbols xs)
          get_symbols (LetExpression _ x) = get_symbols x
          get_symbols (ListExpression x) = concat (map get_symbols x)


tiProgram :: Program -> [Assumption]
tiProgram (Program bgs) = 
    
    runTI $ do assume$ "true" :>: (Forall [] ([] :=> Type (TypeConst "Bool" Star)))
               assume$ "false" :>: (Forall [] ([] :=> Type (TypeConst "Bool" Star)))
               assume$ "error" :>: (Forall [Star] ([] :=> TypeGen 0))
              -- tiTypeDefs bgs
               mapM tiBindGroup$ to_group bgs
               s  <- get_substitution
               ce <- get_classenv
               ps <- get_predicates
               as <- get_assumptions
               rs <- reduce ce (apply s ps)
               return (apply s as)

    where to_group :: [Statement] -> [BindGroup]
          to_group [] = []
          to_group xs = case takeWhile not_module xs of
                          [] -> to_group' xs
                          yx -> sort_deps (foldl f (Scope [] [] [] []) xs) : to_group' (dropWhile not_module xs)

          to_group' [] = []
          to_group' (ModuleStatement x y:xs) = Module (show x) (to_group y) : to_group xs
          to_group' _ = error "Unexpected"

          sort_deps (Scope t a b c) = Scope t a (sort_dep b) c

          not_module (ModuleStatement _ _) = False
          not_module _ = True

          f (Scope t a b c) (DefinitionStatement x @ (Definition _ (EqualityAxiom _ _:_))) = Scope t a (b ++ [[x]]) c
          f (Scope t a b c) (DefinitionStatement x @ (Definition _ (TypeAxiom _:_))) = Scope t (a ++ [x]) b c
          f (Scope t a b c) (ExpressionStatement _ x) = Scope t a b (c ++ [x])
          f (Scope t a b c) x @ (TypeStatement _ _) = Scope (t ++ [x]) a b c
          f x _ = x

