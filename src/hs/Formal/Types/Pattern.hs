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

module Formal.Types.Pattern where

import Text.InterpolatedString.Perl6
import Data.Maybe
import Language.Javascript.JMacro

import Control.Applicative
import Control.Monad

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import Text.Parsec.Indent  hiding (same)

import qualified Data.Map as M

import Formal.Parser.Utils
import Formal.Javascript.Utils

import Formal.Types.Literal
import Formal.Types.Type
import Formal.Types.Symbol

import Prelude hiding (curry, (++))





-- Pattern
-- --------------------------------------------------------------------------------

data Match a = Match [Pattern a] (Maybe a)

instance (Show a) => Show (Match a) where
    show (Match p Nothing)  = sep_with " " p
    show (Match p (Just x)) = [qq|{sep_with " " p} when $x|]

instance (Syntax a) => Syntax (Match a) where

    syntax = try conditional <|> ((\x -> Match x Nothing) <$> (try jStyle <|> hStyle))

        where 

          jStyle = do x <- indentPairs "(" (syntax `sepEndBy1` comma) ")"
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
    toJExpr (PM _ AnyPattern) = toJExpr True

    toJExpr (PM n (VarPattern x)) = toJExpr True

    toJExpr (PM n (LiteralPattern x)) =

        [jmacroE| `(ref n)` === `(x)` |]

    toJExpr (PM _ (RecordPattern (M.toList -> []) Complete)) =

        [jmacroE| true |]

    toJExpr (PM n (RecordPattern (M.toList -> xs) _)) =

        [jmacroE| `(map g xs)` && `(map f xs)` |]
            where f (key, val) = PM (n ++ "[\"" ++ to_name key ++ "\"]") val
                  g (key, _) = Condition [jmacroE| typeof `(ref n)`[`(to_name key)`] != __undefined__ |]

    toJExpr (PM n (ListPattern [])) =
        [jmacroE| equals(`(n)`)([]) |]
    toJExpr (PM n (ListPattern xs)) = 
        let x = toJExpr (map f (zip [(0 :: Int) ..] xs))
            f (index, val) = toJExpr (PM (n ++ "[" ++ show index ++ "]") val)
        in   [jmacroE| `(x)` && `(ref n)`.length == `(length xs)` |]
    toJExpr (PM _ x) = error $ "Unimplemented " ++ show x

data Partial = Partial | Complete deriving (Eq, Show)

data Pattern a = VarPattern String
               | AnyPattern
               | LiteralPattern Literal
               | RecordPattern (M.Map Symbol (Pattern a)) Partial
               | ListPattern [Pattern a]
               | ViewPattern a (Pattern a)

instance (Show a) => Show (Pattern a) where
    show (VarPattern x)     = x
    show AnyPattern         = "_"
    show (LiteralPattern x) = show x
    show (ListPattern x)    = [qq|[ {sep_with ", " x} ]|]
    show (ViewPattern x y)  = [qq|($x -> $y)|]
    show (RecordPattern m Complete)  = [qq|\{ {unsep_with " = " m} \}|] 
    show (RecordPattern m Partial)   = [qq|\{ {unsep_with " = " m}, _ \}|] 

instance (Show a, ToJExpr a) => ToJExpr [Pattern a] where
    toJExpr ps = toJExpr$ zipWith PM (reverse . take (length ps) . map local_pool $ [0 .. 26]) ps

instance (Syntax a) => Syntax (Pattern a) where
    
    syntax = try literal
             <|> try var
             <|> any'
             <|> try record
             <|> naked_apply
             <|> array
             <|> list
             <|> indentPairs "(" (try view <|> syntax) ")"

        where view        = ViewPattern <$> syntax <* spaces <* string "->" <* whitespace <*> syntax 
              var         = VarPattern <$> type_var
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



