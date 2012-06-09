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
import Language.Javascript.JMacro

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (lift)

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import Text.Parsec.Indent  hiding (same)
import Text.Parsec.Expr

import qualified Data.Map as M

import Formal.Parser.Utils
import Formal.Javascript.Utils

import Formal.Types.Literal
import Formal.Types.Type
import Formal.Types.Symbol

import Data.String.Utils hiding (join)
import Data.Monoid

import Prelude hiding (curry, (++))





-- Pattern
-- --------------------------------------------------------------------------------

data Match a = Match [Pattern a] (Maybe a)

instance (Show a) => Show (Match a) where
    show (Match p Nothing)  = sep_with " " p
    show (Match p (Just x)) = [qq|{sep_with " " p} when $x|]

instance (Syntax a) => Syntax (Match a) where

    syntax = try conditional <|> ((\x -> Match x Nothing) <$> (syntax `sepEndBy` whitespace1))

        where conditional = do x <- try syntax `sepEndBy` try whitespace1
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
    toJExpr (PM n (VarPattern x)) = 
        [jmacroE| (function() {
                     `(ref x)` = `(ref n)`; 
                     return true; 
                   })() |]

    toJExpr (PM n (LiteralPattern x))               = [jmacroE| `(ref n)` === `(x)` |]
    toJExpr (PM _ (RecordPattern (M.toList -> []) Complete)) = [jmacroE| true |]
    toJExpr (PM n (RecordPattern (M.toList -> xs) _)) = [jmacroE| `(map g xs)` && `(map f xs)` |]
            where f (key, val) = PM (n ++ "[\"" ++ to_name key ++ "\"]") val
                  g (key, _) = Condition [jmacroE| `(ref n)`.hasOwnProperty(`(to_name key)`) |]

    toJExpr (PM n (ListPattern []))                 = [jmacroE| equals(`(n)`)([]) |]
    toJExpr (PM n (ListPattern xs)) = 
        let x = toJExpr (map f (zip [0..] xs))
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
    toJExpr ps = toJExpr $ zipWith PM (reverse . take (length ps) . map local_pool $ [0 .. 26]) ps

instance (Syntax a) => Syntax (Pattern a) where
    
    syntax = try literal
             <|> try naked_apply
             <|> try var
             <|> any'
             <|> record
             <|> array
             <|> list
             <|> indentPairs "(" (try view <|> try apply <|> syntax) ")"

        where view        = ViewPattern <$> syntax <* spaces <* string "->" <* whitespace <*> syntax 
              var         = VarPattern <$> type_var
              literal     = LiteralPattern <$> syntax          
              any'        = many1 (string "_") *> return AnyPattern
              naked_apply = 

                  do x <- many1 letter
                     string ":"
                     return $ RecordPattern (M.fromList [(Symbol x, AnyPattern )]) Complete

              apply = do x <- many1 letter 
                         string ":" 
                         whitespace
                         y <- syntax
                         return $ RecordPattern (M.fromList [(Symbol x, y)]) Complete

              record  = RecordPattern . M.fromList <$> indentPairs "{" pairs' "}"
                                                   <*> option Complete (try (spaces >> char '_' >> many (char '_') >> return Partial))

                  where pairs' = key_eq_val `sepEndBy` try (comma <|> not_comma)
                        key_eq_val = do key <- syntax
                                        spaces
                                        string "=" <|> string ":"
                                        spaces
                                        value <- syntax
                                        return (key, value)

              list = ListPattern <$> indentPairs "[" (syntax `sepBy` try (try comma <|> not_comma)) "]"

              array = f <$> indentAsymmetricPairs "[:" v (try (string ":]") <|> string "]")

                  where v = do whitespace
                               withPos (syntax `sepBy` try (try comma <|> not_comma))

                        f []     = RecordPattern (M.fromList [(Symbol "nil", AnyPattern)]) Complete
                        f (x:xs) = RecordPattern (M.fromList [(Symbol "head", x), (Symbol "tail", f xs)]) Complete



