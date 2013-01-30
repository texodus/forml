
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

module Forml.Deps where

import qualified Data.Map as M

import Data.List

import           Forml.Types.Axiom
import           Forml.Types.Definition
import           Forml.Types.Expression
import           Forml.Types.Statement
import           Forml.Types.Namespace hiding (Module)
import           Forml.Types.Pattern
import           Forml.Parser.Utils


import Data.Graph (graphFromEdges, SCC(..))
import Data.Graph.SCC (sccList)

import Language.Javascript.JMacro


sort_dep :: [[Definition]] -> [[Definition]]
sort_dep [] = []
sort_dep (concat -> xs) = unwrap `map` sccList graph

    where (graph, reverse_lookup, _) = graphFromEdges . map to_node $ xs 

          unwrap (AcyclicSCC v) = [ get_node . reverse_lookup $ v ]
          unwrap (CyclicSCC v)  = map (get_node . reverse_lookup) v  
    
          get_node (d, _, _) = d
          
          to_node :: Definition -> (Definition, String, [String])
          to_node def @ (Definition _ _ n as) =
              (def, show n, concat . map get_symbols . get_expressions $ as)
                 
          get_expressions [] = []
          get_expressions (TypeAxiom _: xs') = get_expressions xs'
          get_expressions (EqualityAxiom (Match _ (Just y)) (Addr _ _ x): xs') = y : x : get_expressions xs'
          get_expressions (EqualityAxiom _ (Addr _ _ x): xs') = x : get_expressions xs'

          get_symbols (RecordExpression (unzip . M.toList -> (_, xs))) = concat (map get_symbols xs)
          get_symbols (AccessorExpression (Addr _ _ x) _) = get_symbols x
          get_symbols (ApplyExpression a b) = get_symbols a ++ concat (map get_symbols b)
          get_symbols (IfExpression a b (Just c)) = get_symbols a ++ get_symbols b ++ get_symbols c
          get_symbols (IfExpression a b Nothing) = get_symbols a ++ get_symbols b
          get_symbols (LiteralExpression _) = []
          get_symbols (SymbolExpression x) = [show x]
          get_symbols (JSExpression x) = get_jexpr x
          get_symbols (LazyExpression (Addr _ _ x) _)      = get_symbols x
          get_symbols (FunctionExpression as) = concat$ map get_symbols$ get_expressions as
          get_symbols (LetExpression xs x) = (concat . map get_symbols . concat . map get_expressions . map (\(Definition _ _ _ as) -> as) $ xs) ++ get_symbols x
          get_symbols (ListExpression x) = concat (map get_symbols x)
          get_symbols _ = error "Unimplemented TypeCheck 544"

          get_stat (ReturnStat x)      =  (get_jexpr x)
          get_stat (IfStat a b c)      =  (get_jexpr a) ++ (get_stat b) ++ (get_stat c)
          get_stat (WhileStat a b c)   =  (get_jexpr b) ++ (get_stat c)
          get_stat (ForInStat a b c d) =  (get_jexpr c) ++ (get_stat d)
          get_stat (SwitchStat a b c)  =  (get_jexpr a) ++ (get_stat c)
          get_stat (TryStat a b c d)   =  (get_stat a) ++ (get_stat c) ++ (get_stat d)
          get_stat (BlockStat xs)      =  concat (get_stat `map` xs)
          get_stat (ApplStat a b)      =  (get_jexpr a) ++ concat (get_jexpr `map` b)
          get_stat (PPostStat a b c)   =  (get_jexpr c)
          get_stat (AssignStat a b)    =  (get_jexpr a) ++ (get_jexpr b)
          get_stat (UnsatBlock a)      =  [] --(get_stat `fmap` a)
          get_stat (DeclStat v t) = []
          get_stat (UnsatBlock ident_supply) = [] --get_stat `fmap` ident_supply
          get_stat (AntiStat s) = []
          get_stat (ForeignStat s t) = []
          get_stat (BreakStat s) = []

          get_jval (JList xs)   = concat $ get_jexpr `map` xs
          get_jval (JHash m)    = concat $ M.elems $ M.map get_jexpr m
          get_jval (JFunc xs x) = get_stat x
          get_jval (UnsatVal x) = [] -- get_jexpr `fmap` x
          get_jval x@(JDouble _) = []
          get_jval x@(JInt _) = []
          get_jval x@(JStr _) = []
          get_jval x@(JRegEx _) = []
          get_jval (JVar (StrI x)) = [x]


          get_jexpr (SelExpr e (StrI i))  = get_jexpr e
          get_jexpr (IdxExpr a b)         = get_jexpr a ++ get_jexpr b
          get_jexpr (InfixExpr a b c)     = get_jexpr b ++ get_jexpr c
          get_jexpr (PPostExpr a b c)     = get_jexpr c
          get_jexpr (IfExpr a b c)        = get_jexpr a ++ get_jexpr b ++ get_jexpr c
          get_jexpr (NewExpr a)           = get_jexpr a
          get_jexpr (ApplExpr a b)        = get_jexpr a ++ concat (get_jexpr `map` b)
          get_jexpr (TypeExpr a b c)      = get_jexpr b
          get_jexpr (ValExpr a)           = get_jval a
          get_jexpr (UnsatExpr a)         = [] --get_jexpr `fmap` a




sorted_defs :: [Statement] -> [Statement]
sorted_defs [] = []
sorted_defs xs =
    
    case takeWhile not_module xs of
        [] -> (take 1 xs) ++ sorted_defs (drop 1 xs)
        yx -> sort_deps yx ++ sorted_defs (dropWhile not_module xs)

    where 

        sort_deps ys = 

            rejoin (concat $ sort_dep (get_defs ys)) ys
            
            where get_defs (DefinitionStatement d : xs) = [d] : get_defs xs
                  get_defs (x : xs) = get_defs xs
                  get_defs [] = []

                  rejoin (x:xs) (DefinitionStatement _ : ys) = DefinitionStatement x : rejoin xs ys
                  rejoin xs (y:ys) = y : rejoin xs ys
                  rejoin [] [] = []
                  rejoin _ _ = error "Error sorting dependencies"

                

        not_module (ModuleStatement _ _) = False
        not_module _ = True





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


