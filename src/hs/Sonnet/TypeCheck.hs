{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Sonnet.TypeCheck(unify, typeCheck, TypeEquation, verify) where

import Control.Applicative
import Control.Monad.State
import Language.Javascript.JMacro

import Data.Char
import Data.List

import Sonnet.AST
import Sonnet.Parser

import qualified Data.Map as M
import qualified Data.Set as S




--------------------------------------------------------------------------------
--
-- Type Checking / Inference

data TypeEquation = TypeEquation Type Type deriving Eq
type TypeEnv      = M.Map String Type

instance Show TypeEquation where
  show (TypeEquation t u) = show t ++ " = " ++ show u

instance Ord TypeEquation where
  compare (TypeEquation a b) (TypeEquation x y) | a == y && b == x = EQ
  compare x y = compare (show x) (show y)

genUniqueType :: State Int Type
genUniqueType =  do x <- get
                    put (x + 1)
                    return (UnknownType x)

genUniqueInstance :: Type -> State Int Type
genUniqueInstance    (TypeVar y) =  do x <- get
                                       put (x + 1)
                                       return (TypeInstance y x)
genUniqueInstance x = return x

instantiate :: Type -> State Int Type
instantiate    (FunType x y) = FunType <$> instantiate x <*> instantiate y
instantiate    x                  = genUniqueInstance x

typeCheck :: [Definition] -> [TypeEquation]
typeCheck    defs         =  fst $ runState (typeCheckState defs) 0

typeCheckState :: [Definition] -> State Int [TypeEquation]
typeCheckState    defs
  =  do (symbols, types) <- genInitEnv defs
        eqs <- sequence $ checkDef (mapEnv symbols types) <$> zipWith (,) defs types
        return $ concat eqs

  where mapEnv []     []     = M.empty
        mapEnv (x:xs) (y:ys) = M.insert x y (mapEnv xs ys)

        genInitEnv []                  = return ([], [])
        genInitEnv (Struct i t:ds)     = do (x, y) <- genInitEnv ds
                                            return (i : x, t : y)
        genInitEnv (Definition i _:ds) = do utype  <- genUniqueType
                                            (x, y) <- genInitEnv ds
                                            return $ (i : x, utype : y)

checkDef :: TypeEnv -> (Definition, Type)             -> State Int [TypeEquation]
checkDef    env        ((Definition i axs),        t) =  concat <$> (sequence $ map (checkAxiom env t) axs)
checkDef    env        ((Struct i u), t) =  return [] -- The env already maps i = t directly, no need to add a typeEquation

checkAxiom :: TypeEnv -> Type -> Axiom                  -> State Int [TypeEquation]
checkAxiom    env        t       (TypeAxiom u)          =  return [TypeEquation t u]
checkAxiom    env        t       (AssertAxiom _ _)      =  return []
checkAxiom    env        t       (RelationalAxiom ps e)
  =  do newEnv     <- augmentEnv env ps
        resultType <- genUniqueType
        funType    <- genFunType newEnv ps resultType
        (([TypeEquation t funType] ++ checkPatternBindings newEnv ps) ++) <$> checkExpression newEnv e resultType

genFunType :: TypeEnv -> [Pattern]                   -> Type     -> State Int Type
genFunType    env        []                             t        =  return t
genFunType    env        (LiteralPattern (NumLiteral i):ps) t    =  FunType (Type "Num") <$> genFunType env ps t
genFunType    env        (LiteralPattern (StringLiteral i):ps) t =  FunType (Type "String") <$> genFunType env ps t
genFunType    env        (IgnorePattern:ps) t
  =  do utype <- genUniqueType
        FunType utype <$> genFunType env ps t
genFunType    env        (VarPattern i:ps) t
  =  case M.lookup i env of
        Nothing -> return $ InvalidType $ "Symbol " ++ i ++ " is not defined (This is impossible, augmentEnv should have populated me!)"
        Just x  -> FunType x <$> genFunType env ps t
genFunType    env        (BindPattern i ps:pss) t
  =  case M.lookup i env of
        Nothing -> return $ InvalidType $ "Symbol " ++ i ++ " is not defined (This is impossible, augmentEnv should have populated me!)"
        Just x  -> FunType (getResultType x) <$> genFunType env pss t

getResultType (FunType x y) = getResultType y
getResultType x             = x

augmentEnv :: TypeEnv -> [Pattern]               -> State Int TypeEnv
augmentEnv    env        []                      =  return env
augmentEnv    env        (LiteralPattern lit:ps) =  augmentEnv env ps
augmentEnv    env        (IgnorePattern:ps)      =  augmentEnv env ps
augmentEnv    env        (BindPattern i ps:pss)  =  augmentEnv env (ps ++ pss)
augmentEnv    env        (VarPattern i:ps)       =  do utype <- genUniqueType
                                                       augmentEnv (M.insert i utype env) ps

checkPatternBindings env (BindPattern i ps:pss) =  case M.lookup i env of
  Nothing -> [ TypeEquation (Type "ERROR") $ InvalidType $ "Symbol " ++ i ++ " is not defined" ] ++ checkPatternBindings env pss
  Just x  -> parsePatternBindings env x ps  ++ checkPatternBindings env pss
checkPatternBindings env (_:ps) = checkPatternBindings env ps
checkPatternBindings env [] = []

parsePatternBindings env (FunType (TypeVar s) xs) _ = error "nyi: type variable bindings"
parsePatternBindings env (FunType x xs) (VarPattern i:ps)= case M.lookup i env of
  Nothing -> [ TypeEquation (Type "ERROR") $ InvalidType $ "Symbol " ++ i ++ " is not defined" ] ++ parsePatternBindings env xs ps
  Just y  -> [ TypeEquation y x ] ++ parsePatternBindings env xs ps
parsePatternBindings env (FunType x xs) (BindPattern i ps:pss)= case M.lookup i env of
  Nothing -> [ TypeEquation (Type "ERROR") $ InvalidType $ "Symbol " ++ i ++ " is not defined" ] ++ parsePatternBindings env xs ps
  Just y  -> parsePatternBindings env y ps ++ parsePatternBindings env xs pss
parsePatternBindings env (FunType x xs) (LiteralPattern (StringLiteral _):ps) = [ TypeEquation x (Type "String") ] ++ parsePatternBindings env xs ps
parsePatternBindings env (FunType x xs) (LiteralPattern (NumLiteral _):ps)    = [ TypeEquation x (Type "Num") ] ++ parsePatternBindings env xs ps
parsePatternBindings env (FunType _ xs) (_:ps) = parsePatternBindings env xs ps
parsePatternBindings env _ [] = []

checkExpression :: TypeEnv -> Expression                         -> Type -> State Int [TypeEquation]
checkExpression    env        (JSExpression _)                      t    =  return [TypeEquation t (Type "IO")]
checkExpression    env        (LiteralExpression (StringLiteral _)) t    =  return [TypeEquation t (Type "String")]
checkExpression    env        (LiteralExpression (NumLiteral _))    t    =  return [TypeEquation t (Type "Num")]
checkExpression    env        (IfExpression cond ex1 ex2) t
  =  do condType <- genUniqueType
        ([ TypeEquation condType (Type "Bool") ] ++)
          <$> concat <$> sequence [ checkExpression env cond condType,
                         checkExpression env ex1 t,
                         checkExpression env ex2 t ]
checkExpression    env        (PrefixExpression i exs) t
  =  do resultType   <- genUniqueType
        argTypes     <- sequence (take (length exs) (repeat genUniqueType))
        functionType <- return $  case M.lookup i env of
          Nothing -> (InvalidType $ "Symbol " ++ i ++ " is not defined")
          Just x  -> x
        ([ TypeEquation t resultType, TypeEquation (genFunType' $ argTypes ++ [resultType]) functionType ] ++)
          <$> concat <$> sequence [checkExpression env x y | (x, y) <- zipWith (,) exs argTypes ]
checkExpression    env        (InfixExpression ex1 (Operator o) ex2) t
  =  do resultType   <- genUniqueType
        t1 <- genUniqueType
        t2 <- genUniqueType
        functionType <- return $ case o of
          "-"  -> FunType (Type "Num") (FunType (Type "Num") (Type "Num"))
          "+"  -> FunType (Type "Num") (FunType (Type "Num") (Type "Num"))
          "==" -> FunType (Type "Num") (FunType (Type "Num") (Type "Num"))
          "/"  -> FunType (Type "Num") (FunType (Type "Num") (Type "Num"))
          "*"  -> FunType (Type "Num") (FunType (Type "Num") (Type "Num"))
          "|"  -> FunType (Type "Bool") (FunType (Type "Bool") (Type "Bool"))
          "&"  -> FunType (Type "Bool") (FunType (Type "Bool") (Type "Bool"))
        ([ TypeEquation t resultType, TypeEquation (genFunType' $ [t1, t2] ++ [resultType]) functionType ] ++)
          <$> concat <$> sequence [checkExpression env ex1 t1, checkExpression env ex2 t2]

genFunType' (x:[]) = x
genFunType' (x:xs) = FunType x (genFunType' xs)


data Substitution = Substitution Type Type
                  deriving (Eq, Show)

unify :: [TypeEquation] -> [TypeEquation]
unify    eqs            = let ans = applySubs (genSubs eqs) eqs
                          in  case S.fromList ans /= S.fromList eqs of
                              True  -> unify ans
                              False -> ans

  where genSubs    eqs = concat $ map genSub eqs

        genSub (TypeEquation i@(UnknownType _) y) | i /= y = [Substitution i y]
        genSub (TypeEquation x@(Type _) i@(UnknownType _)) = [Substitution i x]
        genSub (TypeEquation (FunType x y) (FunType a b))  = Substitution x a : genSub (TypeEquation y b)
        genSub _ = []

        applySubs    [] ts = ts
        applySubs    (s:ss) ts = applySubs ss (concat $ map (applySub s) ts)

        applySub    (Substitution a b) (TypeEquation x y) | a == y && b == x = [TypeEquation x y]
        applySub    (Substitution a b) (TypeEquation x y) | a == x = [TypeEquation x (merge b y)] ++ newRules b y
        applySub    sub (TypeEquation t y) = [TypeEquation t (applySubType sub y)]

        applySubType    (Substitution x y) z | x == z = y
        applySubType    s (FunType a b) = FunType (applySubType s a) (applySubType s b)
        applySubType    _ y = y

        newRules    y@(UnknownType _) x | x /= y = [TypeEquation y x]
        newRules    (Type x) (UnknownType y)     = [TypeEquation (UnknownType y) (Type x)]
        newRules    (FunType a b) (FunType x y)  = (newRules a x) ++ (newRules b y)
        newRules    _              _             = []

        merge    (UnknownType _) x = x
        merge    (FunType a b  ) (FunType x y) = FunType (merge a x) (merge b y)
        merge    x               (UnknownType y)              = x
        merge    (Type x) (Type y) | x == y = Type y
        merge    x@(InvalidType _) _ = x
        merge    _ x@(InvalidType _) = x
        merge    x y = InvalidType $ "Could not resolve types " ++ show x ++ " with " ++ show y

verify :: [TypeEquation] -> Either [String] [String]
verify    eqs = let errors   = [ x | x@(TypeEquation _ t) <- eqs, isValid t ]
                    warnings = [ x | x@(TypeEquation _ t) <- eqs, isWarning t ]
                in case length errors of
                  0 -> Right $ map ((++"\n") . show) warnings
                  n -> Left  $ map ((++"\n") . show) errors

isWarning (UnknownType _) = True
isWarning (FunType x y) = isWarning x || isWarning y
isWarning _ = False

isValid (InvalidType _) = True
isValid (FunType x y) = isValid x || isValid y
isValid _ = False
