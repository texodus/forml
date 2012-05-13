{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Formal.Types.Statement where

import Text.InterpolatedString.Perl6

import Language.Javascript.JMacro

import Control.Applicative

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import Text.Parsec.Indent  hiding (same)

import Data.String.Utils
import Data.Monoid
import qualified Data.List as L

import Formal.Parser.Utils

import Formal.Types.Type
import Formal.Types.TypeDefinition
import Formal.Types.Symbol
import Formal.Types.Definition
import Formal.Types.Expression
import Formal.Types.Namespace

import Formal.Javascript.Utils

import Prelude hiding (curry, (++))



--------------------------------------------------------------------------------
----
---- Statement

data Statement = TypeStatement TypeDefinition UnionType
               | DefinitionStatement Definition
               | ExpressionStatement (SourcePos, SourcePos) (Expression Definition)
               | ImportStatement Namespace
               | ModuleStatement Namespace [Statement]

instance Show Statement where
    show (TypeStatement t c)     = [qq|type $t = $c|]
    show (DefinitionStatement d) = show d
    show (ExpressionStatement _ x) = show x
    show (ImportStatement x) = [qq|import $x|]
    show (ModuleStatement x xs) = replace "\n |" "\n     |" 
                                  $ replace "\n\n" "\n\n    " 
                                  $ "module " 
                                  ++ show x ++ "\n\n" ++ sep_with "\n\n" xs

instance Syntax Statement where

    syntax = whitespace >> withPos statement_types <* many newline

        where statement_types = (try type_statement       <?> "Type Definition")
                                <|> (try import_statement <?> "Import Statement")
                                <|> (try module_statement <?> "Module Declaration")
                                <|> (try def_statement    <?> "Symbol Definition")
                                <|> (expression_statement <?> "Assertion")

              def_statement = DefinitionStatement <$> syntax

              import_statement = do string "open"
                                    whitespace
                                    ImportStatement <$> syntax

              module_statement = do string "module"
                                    whitespace1
                                    name <- syntax
                                    whitespace *> newline
                                    spaces *> (indented <|> same)
                                    ModuleStatement name <$> withPos (many1 ((spaces >> same >> syntax)))

              type_statement  = do string "type"
                                   whitespace1
                                   def <- type_definition
                                   whitespace
                                   sig <- try (string "=" >> spaces >> withPos (string "|" >> type_definition_signature))
                                          <|> withPos (string "=" >> whitespace >> type_definition_signature)
                                   whitespace
                                   return $ TypeStatement def sig

              expression_statement = do whitespace
                                        x <- getPosition 
                                        y <- withPos syntax
                                        z <- getPosition
                                        return $ ExpressionStatement (x, z) y
                                                                   



--------------------------------------------------------------------------------
----
---- Meta

data Meta = Meta { target    :: Target,
                   namespace :: Namespace,
                   modules   :: [Module],
                   expr      :: Statement } deriving (Show)

data Target = Test | Library
            deriving (Show)

instance ToStat Meta where

    -- Expressions are ignored for Libraries, and rendered as tests for Test
    toStat (Meta { target = Library, expr = ExpressionStatement _ _ }) = mempty
    toStat (Meta { target = Test,    expr = ExpressionStatement (a, b) e }) = 

        [jmacro| it(`(serial ++ "::" ++ show e)`, function() {
                     `(Jasmine e)`;
                 }); |]

            where serial = show (sourceLine a - 1) ++ "_" ++ show (sourceLine b - 1)

    -- Imports work identically for both targets
    toStat (Meta { modules, 
                   namespace = Namespace [], 
                   expr      = ImportStatement target_namespace @ (find modules -> Nothing) }) = 

        error $ [qq| Could not resolve namespace $target_namespace |]  

    toStat (Meta { modules, 
                   expr      = ImportStatement target_namespace, 
                   namespace = (find modules . (++ target_namespace) -> Just y) }) =

        open target_namespace y

    toStat meta @ (Meta { expr = ImportStatement _, .. }) = 

        let slice (Namespace ns) = Namespace . take (length ns - 1) $ ns
        in  toStat (meta { namespace = slice namespace })

    -- Modules in test mode must open the contents of the Library
    toStat meta @ (Meta { target = Library, namespace = Namespace [], expr = ModuleStatement ns xs, .. }) =
        
        let ex = [jmacroE| new (function() { 
                               `(fmap (\z -> meta { namespace =  ns, expr = z }) xs)`;
                           }) |]

        in declare_window (show ns) ex

    toStat meta @ (Meta { target = Library, expr = ModuleStatement ns xs, .. }) =
        
        let ex = [jmacroE| new (function() { 
                               `(fmap (\z -> meta { namespace = namespace ++ ns, expr = z }) xs)`;
                           }) |]

        in declare_this (show ns) ex

    toStat meta @ (Meta { target = Test, expr = ModuleStatement ns xs, .. }) =

        let (imports, rest) = L.partition f xs
            f (ImportStatement _) = True
            f _ = False in

        [jmacro| describe(`(show ns)`, function() {
                     `(map (\z -> meta { namespace = namespace ++ ns, expr = z }) imports)`; 
                     `(open (namespace ++ ns) xs)`;
                     var x = new (function {
                         `(map (\z -> meta { namespace = namespace ++ ns, expr = z }) rest)`; 
                     }());
                 }); |]

    -- Definitions are ignored for the Test target
    toStat (Meta { target = Library, expr = DefinitionStatement d }) = toStat d
    toStat (Meta { target = Test,    expr = DefinitionStatement _ }) = mempty

    toStat (Meta { expr = TypeStatement _ _ }) = mempty

    toStat x = error $ "Unimplemented " ++ show x

empty_meta :: Target -> [Statement] -> Statement -> Meta
empty_meta x = Meta x (Namespace []) . build_modules

    where build_modules (ModuleStatement n ns : xs) = Module n (build_modules ns) : build_modules xs
          build_modules (DefinitionStatement (Definition n _): xs) = Var (to_name n) : build_modules xs
          build_modules (_ : xs) = build_modules xs
          build_modules [] = []

find :: [Module] -> Namespace -> Maybe [String]
find (Var s : ss) n @ (Namespace []) = Just [s] ++ find ss n
find (Var _: xs) ns                  = find xs ns
find [] (Namespace [])               = Just []
find [] _                            = Nothing

find (Module _ xs : ms) n @ (Namespace []) = Just (map show xs) ++ find ms n
find (Module (Namespace ys) x : zs) n @ (Namespace xs)
     | length xs >= length ys && take (length ys) xs == ys = 
         find x (Namespace $ drop (length ys) xs)
     | otherwise = find zs n



--------------------------------------------------------------------------------
----
---- Open

class Open a where open :: Namespace -> [a] -> JStat

instance Open Statement where
    open _ [] = mempty
    open ns (DefinitionStatement (Definition n _) : xs) =

        let f = ref . show
            x = [jmacroE| `(f ns)`[`(n)`] |] in

        [jmacro| `(declare (to_name n) x)`;
                 `(open ns xs)`; |]

    open nss (ModuleStatement ns @ (Namespace (n:_)) _:xs) =

        [jmacro| `(declare n (ref . show $ nss ++ ns))`;
                 `(open nss xs)`; |]

    open ns (_ : xs) = open ns xs

instance Open String where
    open _ [] = mempty
    open (Namespace ns) (x:xs) =

        let print' []     = error "Empty Namespace"
            print' (y:[]) = [jmacroE| `(ref y)` |]
            print' (y:ys) = [jmacroE| `(print' ys)`[`(y)`] |]

        in  declare x [jmacroE| `(print' $ reverse ns)`[`(x)`] |] ++ open (Namespace ns) xs




--------------------------------------------------------------------------------
----
---- Jasmine

newtype Jasmine = Jasmine (Expression Definition)

instance ToStat Jasmine where

    toStat (Jasmine (ApplyExpression (SymbolExpression (Operator "==")) [x, y])) =

        [jmacro| expect(`(x)`).toEqual(`(y)`); |]

    toStat (Jasmine (ApplyExpression (SymbolExpression (Operator "!=")) [x, y])) =

        [jmacro| expect(`(x)`).toNotEqual(`(y)`); |]
        
    toStat (Jasmine e) = 

        [jmacro| expect((function() {
                     try {
                         return `(e)`;
                     } catch (ex) {
                         return { error: ex };
                     }
                 })()).toEqual(true); |]


