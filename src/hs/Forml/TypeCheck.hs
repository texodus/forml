
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Forml.TypeCheck where

import           Data.List                     (intersect, partition,
                                                union, (\\))

import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Data.Set                      as S

import           Text.ParserCombinators.Parsec

import           Forml.Types.Axiom
import           Forml.Types.Definition
import           Forml.Types.Expression
import           Forml.Types.Namespace        hiding (Module)
import           Forml.Types.Pattern
import           Forml.Types.Statement        hiding (Test, find, modules,
                                                namespace)
import           Forml.Types.Symbol

import           Forml.Types.Type
import           Forml.Types.TypeDefinition

import           Forml.Parser
import           Forml.Parser.Utils

import           Forml.TypeCheck.Types
import Data.Graph (graphFromEdges, SCC(..))
import Data.Graph.SCC (sccList)


-- Type Inference
-- --------------------------------------------------------------------------------


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

    infer (RecordExpression (unzip . M.toList -> (names, xs))) =

        do ts <- mapM infer xs
           let r = TypeRecord (TRecord (M.fromList (zip (map f names) ts)) TComplete Star)
           t' <- newTVar Star
           sc <- find $ quantify (tv r) ([] :=> r)
           case sc of
             Nothing ->
                 do unify t' r
                    return t'
             Just (Forall _ scr, sct) ->
                 do (_ :=> t'') <- freshInst sct
                    (qs :=> t''') <- return$ inst (map TypeVar$ tv t'') (scr)
                    (_ :=> t) <- freshInst (quantify (tv t''' \\ tv t'') (qs :=> t'''))
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

        with_scope$ do infer' defs
                       infer x

        where defs = to_group (map DefinitionStatement xs)

              infer' [] = return []
              infer' (x:xs) =

                 do a <- infer x
                    assume a
                    as <- infer' xs
                    return$ a ++ as

    infer (ListExpression x) =

        do t <- newTVar Star
           ts <- mapM infer x
           mapM (unify t) ts
           t' <- newTVar Star
           unify t' (TypeApplication (Type (TypeConst "Array" (KindFunction Star Star))) t)
           return t'
           
    infer x = error $ "Unimplemented: " ++ show x

-- Axioms

instance (Infer a t) => Infer (Addr a) t where

    infer (Addr s _ x) = do m <- get_msg
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
split ce fs _ ps =

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
              simple (Definition _ _ _ axs) = any (null . f) axs

              f (EqualityAxiom (Match p _) _) = p
              f _ = error "Fatal error occurred while reticulating splines"


data BindGroup =

    Scope {
         imports :: [(Namespace, Maybe String)],
         statements :: [Statement],
         explicits :: [Definition],
         implicits :: [[Definition]],
         tests :: [Addr (Expression Definition)]
     } 

   | Module String [BindGroup]

   deriving (Show)

instance Infer Definition () where

    infer (Definition _ _ name axs) =

        do sc <- find$ f name
           (qs :=> t)  <- freshInst sc
           axiom_types <- with_scope$ mapM (with_scope . infer) axs

           s <- get_substitution
           mapM (flip unify t) axiom_types  -- TODO apply sub to axiom_types?

           as <- get_assumptions
           ce <- get_classenv
           ps <- get_predicates

           let qs' = apply s qs
               t'  = apply s t
               fs  = tv (apply s as)
               gs  = tv t' \\ fs
               sc' = quantify gs (qs' :=> t')
               ps' = filter (not . entail ce qs') (apply s ps)

           (_, rs) <- split ce fs gs ps'

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

instance Infer BindGroup [Assumption] where
    infer (Scope imps tts es iss ts) =

        do --as <- get_assumptions
           mapM import' imps
           as' <- get_assumptions
           infer tts
           mapM assume$ sigs es
           mapM infer iss
           with_scope$ mapM infer es
           mapM infer (map Test ts)
           as'' <- get_assumptions
           set_assumptions as'
           return (as'' \\ as')

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

              import' (Namespace ns, Nothing) =

                  do z <- get_modules
                     a <- get_assumptions
                     (Namespace ns') <- get_namespace
                     case Namespace ns `lookup` z of
                       Just z' -> assume$ a ++ z'
                       Nothing -> if length ns' > 0 && head ns' /= head ns
                                  then import' (Namespace (head ns' : ns), Nothing)
                                  else add_error$ "Unknown namespace " ++ show (Namespace ns)

              import' (Namespace ns, Just alias) =

                  do z <- get_modules
                     a <- get_assumptions
                     (Namespace ns') <- get_namespace
                     case Namespace ns `lookup` z of
                       Just z' -> 
                           do record <- to_record z'
                              assume $ alias :>: record
                       Nothing -> if length ns' > 0 && head ns' /= head ns
                                  then import' (Namespace (head ns' : ns), Just alias)
                                  else add_error$ "Unknown namespace " ++ show (Namespace ns)
                                  
              to_record assumptions =
              
                  let f (_ :>: scheme) =

                          [ do (_ :=> t) <- freshInst scheme
                               return t ]

                      f _ = []
                      
                      g (i :>: _) = [i]
                      g _ = []
                      
                  in  do schemes <- sequence $ concat $ map f assumptions
                         let symbols = concat $ map g assumptions 
                         let rec = TypeRecord (TRecord (M.fromList (zip symbols schemes)) TComplete Star)
                         return $ quantify (tv rec) ([] :=> rec)


              h (Symbol x) = x
              h (Operator x) = x

    infer (Module name bgs) =

        do as <- get_assumptions
           with_module name$ infer' bgs
           set_assumptions as
           return []

         where infer' [] = return []
               infer' (x:xs) =

                  do a <- infer x
                     assume a
                     as' <- infer' xs
                     return$ a ++ as'



to_scheme :: TypeDefinition -> UnionType -> [Assumption]
to_scheme (TypeDefinition n vs) t = [ quantify (vars y) ([]:=> y) :>>: def_type y
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
                                   else error "Kind mismatch in scheme"

          infer_kinds x (TypeApplication a b) = infer_kinds x a ++ infer_kinds x b
          infer_kinds x (TypeVar (TVar y k)) | x == y = [k]
          infer_kinds x (TypeRecord (TRecord m _ _)) = concat$ map (infer_kinds x) (M.elems m)
          infer_kinds _ _ = []

-- | Computes all possible types from a type signature AST.

enumerate_types :: UnionType -> [Type]
enumerate_types (UnionType types) = to_unit . concat . map enumerate_type . S.toList $ types

    where term_type (VariableType x)      = [ TypeVar (TVar x Star) ]
          term_type (SymbolType x)        = [ Type (TypeConst (show x) Star) ]
          term_type (PolymorphicType a b) = [ foldl TypeApplication a' b'
                                                  | b' <- map enumerate_types b
                                                  , a' <- to_kind' (length b')$ term_type a ]

          to_kind 0 = Star
          to_kind n = KindFunction Star (to_kind$ n - 1)

          to_unit [] = [TypeRecord (TRecord M.empty TComplete Star)]
          to_unit x = x

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





instance Infer [Statement] () where

    infer [] = return ()
    infer (TypeStatement t c : xs) =

        do assume     $ to_scheme t c
           infer xs

    infer (_ : xs) = infer xs

sort_dep :: [[Definition]] -> [[Definition]]
sort_dep [] = []
sort_dep (concat -> xs) = free

    where free = unwrap `map` sccList graph
    
              where (graph, reverse_lookup, _) = graphFromEdges . get_nodes $ xs 

                    unwrap (AcyclicSCC v) = [ get_node . reverse_lookup $ v ]
                    unwrap (CyclicSCC v)  = map (get_node . reverse_lookup) v  
    
                    get_node (d, _, _) = d
                    
                    get_nodes :: [Definition] -> [(Definition, String, [String])]
                    get_nodes = map to_node
                    
                    to_node :: Definition -> (Definition, String, [String])
                    to_node def @ (Definition _ _ n as) =
                        (def, show n, concat $ get_symbols `map` get_expressions' as)
                 
          get_names :: [Definition] -> [String]
          get_names [] = []
          get_names (Definition _ _ n _:xs) = show n : get_names xs

          get_expressions :: [Definition] -> [[Expression Definition]]
          get_expressions [] = []
          get_expressions (Definition _ _ _ as : xs) = get_expressions' as : get_expressions xs

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


js_type :: Type
js_type = Type (TypeConst "JS" (KindFunction Star Star))

tiProgram :: Program -> [(Namespace, [Assumption])] -> ([(Namespace, [Assumption])], [String])
tiProgram (Program bgs) env =

    runTI $ do TI (\x -> (x { modules = env }, ()))
               assume$ "true"  :>: (Forall [] ([] :=> Type (TypeConst "Bool" Star)))
               assume$ "false" :>: (Forall [] ([] :=> Type (TypeConst "Bool" Star)))
               assume$ "error" :>: (Forall [Star] ([] :=> TypeGen 0))
               assume$ "run"   :>: (Forall [Star] ([] :=> (TypeApplication js_type (TypeGen 0) -:> TypeGen 0)))
               infer'$ to_group bgs
               s  <- get_substitution
               ce <- get_classenv
               ps <- get_predicates
               ms <- TI (\y -> (y, modules y))
               rs <- reduce ce (apply s ps)
               e  <- get_errors
               return ((apply s ms), S.toList . S.fromList $ e)

    where infer' [] = return ()
          infer' (x:xs) =

              do a <- infer x
                 assume a
                 infer' xs




to_group :: [Statement] -> [BindGroup]
to_group [] = []
to_group xs =

    case takeWhile not_module xs of
        [] -> to_group' xs
        yx -> sort_deps (foldl f (Scope [] [] [] [] []) yx)
                   : to_group' (dropWhile not_module xs)

    where to_group' [] = []
          to_group' (ModuleStatement x y:ys) = Module (show x) (to_group y) : to_group ys
          to_group' _ = error "Unexpected"

          sort_deps s @ Scope { implicits = b } = s { implicits = sort_dep b }

          not_module (ModuleStatement _ _) = False
          not_module _ = True

          f s @ Scope { implicits = b} (DefinitionStatement x @ (Definition _ _ _ (EqualityAxiom _ _:_))) =
              s { implicits = b ++ [[x]] }
          f s @ Scope { explicits = a} (DefinitionStatement x @ (Definition _ _ _ (TypeAxiom _:_))) =
              s { explicits = a ++ [x] }
          f s @ Scope { tests = c } (ExpressionStatement x) = s { tests = c ++ [x] }
          f s @ Scope { imports = i } (ImportStatement ns Nothing) = s { imports = i ++ [(ns, Nothing)] }
          f s @ Scope { imports = i } (ImportStatement ns (Just alias)) = s { imports = i ++ [(ns, Just alias)] }
          f s @ Scope { statements = t } x @ (TypeStatement _ _) = s { statements = t ++ [x] }
          f x _ = x

