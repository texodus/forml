{-# LANGUAGE FlexibleContexts, NamedFieldPuns, OverlappingInstances,
             QuasiQuotes, RankNTypes, RecordWildCards, TemplateHaskell,
             ViewPatterns #-}

module Forml.Types.Type where

import Control.Applicative
import Text.InterpolatedString.Perl6

import Text.Parsec        hiding (State, label, many, parse, spaces, (<|>))
import Text.Parsec.Indent hiding (same)

import qualified Data.Map as M
import qualified Data.Set as S

import Forml.Parser.Utils

import Forml.Types.Symbol


data UnionType = UnionType (S.Set ComplexType)
               deriving (Ord, Eq)

data ComplexType = RecordType (M.Map Symbol UnionType)
                 | InheritType SimpleType (M.Map Symbol UnionType)
                 | FunctionType UnionType UnionType
                 | SimpleType SimpleType
                 deriving (Eq, Ord)

data SimpleType = PolymorphicType SimpleType [UnionType]
                | SymbolType Symbol
                | VariableType String
                deriving (Ord, Eq)

instance Show UnionType where
    show (UnionType xs)         = [qq|{sep_with " | " $ S.toList xs}|]

instance Show ComplexType where
    show (SimpleType y)         = [qq|$y|]
    show (InheritType n m)      = [qq|\{ $n with {unsep_with ": " m} \}|]
    show (RecordType m)         = [qq|\{ {unsep_with ": " m} \}|]

    show (FunctionType g @ (UnionType (S.toList -> ((FunctionType _ _):[]))) h) =
        [qq|($g -> $h)|]

    show (FunctionType g h)     = [qq|$g -> $h|]

instance Show SimpleType where
    show (PolymorphicType x y)  = [qq|($x {sep_with " " y})|]
    show (SymbolType x)   = show x
    show (VariableType x) = x



-- Type Signatures
-- -----------------------------------------------------------------------------
-- TODO ! (IO type)
-- TODO type axioms need nominative types?
-- ? TODO List, Map, Set shorthand?

-- The type algebra of Sonnet is broken into 3 types to preserve the
-- associativity of UnionTypes: (x | y) | z == x | y | z



-- Where a type signature may be used in Sonnet had two slightly different parsers
-- in order to allow for somewhat overloaded surrounding characters (eg "|" - when
-- declaring the type of an axiom, one must be careful to disambiguate UnionTypes
-- and sets of EqualityAxioms).  However, these types are otherwise equivalent,
-- and any type that may be declared in a TypeDefinition may also be the explicit
-- type of a Definition (Note, however, that in the case of NamedTypes, the
-- names introduced into scope will be inaccessible in the case of a Definition).


type_axiom_signature      :: Parser UnionType
type_definition_signature :: Parser UnionType

type_axiom_signature = do option "" (string "|" <* whitespace)
                          t <- (u $ try function_type) <|> try nested_union_type <|> u inner_type
                          whitespace
                          return t

    where u = (<$>) $ UnionType . S.fromList . (:[])

type_definition_signature = UnionType . S.fromList <$> types <* whitespace
    where types = (try function_type <|> inner_type) `sepBy1` type_sep


-- Through various complexities of the recursive structure of these types, we will
-- need a few mutually recursive parsers to express these slightly different
-- signature parsers.

inner_type        :: Parser ComplexType
nested_function   :: Parser ComplexType
nested_union_type :: Parser UnionType

inner_type  = nested_function
              <|> try record_type
              <|> try named_type
              <|> try poly_type
              <|> var_type
              <|> symbol_type

nested_function   = indentPairs "(" (try function_type <|> inner_type) ")"
nested_union_type = indentPairs "(" type_definition_signature ")"


-- Now that we've expressed the possible parses of a UnionType, we can move on to
-- parsing the ComplexType and SimpleType layers.  While these are also mutually
-- recursive, the recursion is uniform, as the various allowable combinations
-- have already been defined above.

function_type :: Parser ComplexType
poly_type     :: Parser ComplexType
record_type   :: Parser ComplexType
named_type    :: Parser ComplexType
symbol_type   :: Parser ComplexType
var_type      :: Parser ComplexType

function_type = do x <- try nested_union_type <|> unionize inner_type
                   spaces
                   string "->" <|> string "→"
                   spaces
                   y <- (unionize $ try function_type) <|> try nested_union_type <|> unionize inner_type
                   return $ FunctionType x y

poly_type = do name <- (SymbolType <$> type_name) <|> (VariableType <$> try type_var)
               whitespace
               SimpleType . PolymorphicType name <$> (j_style <|> h_style)

     where rvs = record_type <|> var_type <|> symbol_type
           h_style = (try nested_union_type <|> unionize (try rvs)) `sepEndBy1` whitespace
           j_style = indentPairs "<" ((try nested_union_type <|> unionize (try rvs)) `sepBy1` optional_sep) ">"

record_type   = let key_value = (,) <$> syntax <* div' <*> type_definition_signature
                    div'      = spaces <* string ":" <* spaces
                    pairs     = key_value `sepEndBy` optional_sep
                    inner     = RecordType . M.fromList <$> pairs
                    inherit   = do SimpleType n <- try poly_type <|> try symbol_type <|> var_type
                                   spaces
                                   indented
                                   string "with"
                                   spaces *> indented
                                   InheritType n . M.fromList <$> pairs

                in indentPairs "{" (try inherit <|> inner) "}"

named_type = do x <- indentPairs "{" name "}"

-- named_type = do x <- name
--                 y <- option bool (try nested_union_type <|> unionize inner_type)

                return $ RecordType (M.fromList [(Symbol x, bool)])

    where name = type_var
          bool = UnionType (S.fromList [(SimpleType (SymbolType (Symbol "Bool")))])

symbol_type   = SimpleType . SymbolType <$> type_name
var_type      = SimpleType . VariableType <$> type_var

unionize :: Parser ComplexType -> Parser UnionType
unionize = fmap $ UnionType . S.fromList . (:[])


type_name   :: Parser Symbol
type_var    :: Parser String

type_name   = Symbol <$> not_reserved (upper <:> many (alphaNum <|> oneOf "_'"))

type_var    = 
    not_reserved (oneOf "_abscdefghijklmnopqrstuvwxyz" <:> many (alphaNum <|> oneOf "_'")) >>= f
    
    where f "_" = do x <- getPosition
                     return $ "_" ++ show (sourceLine x * sourceColumn x)
          f x   = return x


