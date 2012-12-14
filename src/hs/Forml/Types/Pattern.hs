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
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}

module Forml.Types.Pattern where

import Text.InterpolatedString.Perl6
import Data.Maybe
import Language.Javascript.JMacro

import Control.Applicative
import Control.Monad

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import qualified Text.Parsec as P
import qualified Data.List as L
import Text.Parsec.Indent  hiding (same)

import qualified Data.Map as M

import Forml.Parser.Utils
import Forml.Javascript.Utils

import Forml.Types.Literal
import Forml.Types.Type
import Forml.Types.Symbol
import Forml.TypeCheck.Types

import Prelude hiding (curry, (++))
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid (mappend)





-- Pattern
-- --------------------------------------------------------------------------------

data Match a = Match [Pattern a] (Maybe a) deriving (Eq)
data Pattern a = VarPattern String
               | AnyPattern
               | LiteralPattern Literal
               | RecordPattern (M.Map Symbol (Pattern a)) Partial
               | ListPattern [Pattern a]
               | ViewPattern a (Pattern a)
               | AliasPattern [Pattern a]
               deriving (Eq)


instance (Show a) => Show (Match a) where
    show (Match p Nothing)  = sep_with " " p
    show (Match p (Just x)) = [qq|{sep_with " " p} when $x|]

instance (Syntax a, Show a) => Syntax (Match a) where

    syntax = try conditional <|> ((\x -> Match x Nothing) <$> (try jStyle <|> hStyle))

        where 

          jStyle = do x <- indentPairs "(" (syntax `sepBy1` comma) ")"
                      spaces
                      if length x > 1 then return x else fail "Java-style arguments"

          hStyle = syntax `sepEndBy` whitespace1

          conditional = do x <- try jStyle <|> try hStyle
                           string "when"
                           spaces
                           indented
                           ex <- withPos syntax
                           spaces
                           indented
                           return $ Match x (Just ex)

newtype Condition = Condition JExpr

instance ToJExpr [Condition] where
    toJExpr [] = toJExpr True
    toJExpr (Condition x : []) = [jmacroE| `(x)` |]
    toJExpr (Condition x : xs) = [jmacroE| `(x)` && `(xs)` |]

data PatternMatch a = PM String (Pattern a)

instance (Show a) => ToJExpr [PatternMatch a] where
    toJExpr []     = toJExpr True
    toJExpr (x:[]) = [jmacroE| `(x)` |]
    toJExpr (x:xs) = [jmacroE| `(x)` && `(xs)` |]

instance (Show a) => ToJExpr (PatternMatch a) where

    toJExpr (PM n (AliasPattern xs)) = toJExpr $ filter fil $ map (PM n) xs

    toJExpr (PM n (LiteralPattern x)) =

        [jmacroE| `(ref n)` === `(x)` |]

    toJExpr (PM n (RecordPattern (M.toList -> xs) _)) =

        [jmacroE| `(map g xs)` && `(filter fil $ map f xs)` |]
            where f (key, val) = PM (n ++ "[\"" ++ to_name key ++ "\"]") val
                  g (key, _) = Condition [jmacroE| __check(`(ref n)`, `(to_name key)`) |]

    toJExpr (PM n (ListPattern [])) =
        [jmacroE| equals(`(n)`)([]) |]
    toJExpr (PM n (ListPattern xs)) = 
        let x = toJExpr (map f (zip [(0 :: Int) ..] xs))
            f (index, val) = toJExpr (PM (n ++ "[" ++ show index ++ "]") val)
        in   [jmacroE| `(x)` && `(ref n)`.length == `(length xs)` |]
    toJExpr (PM _ x) = error $ "Unimplemented " ++ show x

data Partial = Partial | Complete deriving (Eq, Show)

instance (Show a) => Show (Pattern a) where
    show (VarPattern x)     = x
    show AnyPattern         = "_"
    show (LiteralPattern x) = show x
    show (ListPattern x)    = [qq|[ {sep_with ", " x} ]|]
    show (ViewPattern x y)  = [qq|($x -> $y)|]
    show (RecordPattern m Complete)  = [qq|\{ {unsep_with " = " m} \}|] 
    show (RecordPattern m Partial)   = [qq|\{ {unsep_with " = " m}, _ \}|]
    show (AliasPattern a) = sep_with " & " a

instance (Show a, ToJExpr a) => ToJExpr [Pattern a] where

    toJExpr ps = toJExpr$ filter fil $ zipWith PM (reverse . take (length ps) . map local_pool $ [0 .. 26]) ps
    
fil (PM _ (VarPattern _)) = False
fil (PM _ AnyPattern) = False
fil (PM _ (RecordPattern (M.toList -> []) Complete)) = False
fil _ = True

instance (Syntax a, Show a) => Syntax (Pattern a) where
    
    syntax = try literal
             <|> try var
             <|> any'
             <|> try record
             <|> naked_apply
             <|> array
             <|> list
             <|> indentPairs "(" (try alias <|> syntax) ")"
                     
        where alias = let sep = P.spaces <* string "&" <* P.spaces
                      in  AliasPattern <$> ((:) <$> syntax <* sep <*> sepBy1 syntax sep) 

              var = 
              
                  do (Symbol x) <- syntax
                     return (VarPattern x)
              
              literal     = LiteralPattern <$> syntax
              any'        = many1 (string "_") *> return AnyPattern
              naked_apply =

                  do x <- indentPairs "{" (many1 letter) "}"
                     return $ RecordPattern (M.fromList [(Symbol x, AnyPattern )]) Complete

              record  = indentPairs "{" any_record "}"
                                                   

                  where pairs = (try key_eq_val <|> (many1 (char '_') >> return Nothing)) `sepEndBy` optional_sep

                        any_record =
                        
                              do ps <- pairs
                                 let ps' = catMaybes ps
                                 return $
                                   case (length ps, (not . isJust) `filter` ps) of
                                     (0, _)           -> RecordPattern M.empty Complete
                                     (_, (Nothing:_)) -> RecordPattern (M.fromList ps') Partial
                                     _                -> RecordPattern (M.fromList ps') Complete

                        key_eq_val = do key <- syntax
                                        spaces
                                        string "=" <|> string ":"
                                        spaces
                                        value <- syntax
                                        return$ Just (key, value)

              list = ListPattern <$> indentPairs "[" (syntax `sepBy` optional_sep) "]"

              array = f <$> indentAsymmetricPairs "[:" v (try (string ":]") <|> string "]")

                  where v = do whitespace
                               withPos (syntax `sepBy` optional_sep)

                        f []     = RecordPattern (M.fromList [(Symbol "nil", AnyPattern)]) Complete
                        f (x:xs) = RecordPattern (M.fromList [(Symbol "head", x), (Symbol "tail", f xs)]) Complete



instance (Show b) => Infer (Pattern b) Type where

    infer (AliasPattern (x:[])) = infer x
           
    infer (AliasPattern (x:xs)) = 
        do z <- infer x
           z' <- infer (AliasPattern xs)
           unify z z'
           return z'

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

    infer (RecordPattern (unzip . M.toList -> (names, patterns)) p) =
        
        do ts <- mapM infer patterns
           p' <- case p of
                   Complete -> return TComplete
                   Partial  -> do t <-  newTVar Star
                                  return$ TPartial t

           let r = TypeRecord (TRecord (M.fromList (zip (map f names) ts)) p' Star)
           t' <- newTVar Star
           sc <- find $ quantify (tv r) ([] :=> r)
           case sc of
             Nothing ->
                 do unify t' r
                    return t'
             Just (Forall _ scr, sct) ->
                 do (_ :=> t'') <- freshInst sct
                    (qs :=> t''') <- return$ inst (map TypeVar$ tv t'') scr
                    (_ :=> t) <- freshInst (quantify (tv t''' L.\\ tv t'') (qs :=> t'''))
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
