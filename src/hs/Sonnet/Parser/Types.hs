{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Sonnet.Parser.Types where

import Text.InterpolatedString.Perl6

import Control.Applicative

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import Text.Parsec.Indent  hiding (same)

import qualified Data.Map as M
import qualified Data.Set as S

import Sonnet.Parser.Utils




-- Type Signatures
-- -----------------------------------------------------------------------------
-- TODO ! (IO type)
-- TODO type axioms need nominative types?
-- ? TODO List, Map, Set shorthand?

-- The type algebra of Sonnet is broken into 3 types to preserve the 
-- associativity of UnionTypes: (x | y) | z == x | y | z


data UnionType = UnionType (S.Set ComplexType)
               deriving (Ord, Eq)

data ComplexType = RecordType (M.Map String UnionType)
                 | InheritType SimpleType (M.Map String UnionType)
                 | FunctionType UnionType UnionType
                 | SimpleType SimpleType
                 | NamedType String (Maybe UnionType)
                 deriving (Eq, Ord)

data SimpleType = PolymorphicType SimpleType [UnionType]
                | SymbolType String 
                | VariableType String
                deriving (Ord, Eq)

instance Show UnionType where 
    show (UnionType xs)         = [qq|{sep_with " | " $ S.toList xs}|]
   
instance Show ComplexType where
    show (SimpleType y)         = [qq|$y|]
    show (NamedType n (Just x)) = [qq|$n: ($x)|]
    show (NamedType n Nothing)  = n ++ ":"
    show (InheritType n m)      = [qq|\{ $n with {unsep_with ": " m} \}|]
    show (RecordType m)         = [qq|\{ {unsep_with ": " m} \}|] 

    show (FunctionType g@(UnionType (S.toList -> ((FunctionType _ _):[]))) h) = [qq|($g -> $h)|]
    show (FunctionType g h)     = [qq|$g -> $h|]

instance Show SimpleType where
    show (PolymorphicType x y)  = [qq|($x {sep_with " " y})|]
    show (SymbolType x)   = x
    show (VariableType x) = x


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
              <|> record_type 
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

function_type = do x <- try nested_union_type <|> lift inner_type
                   spaces
                   string "->" <|> string "→"
                   spaces
                   y <- (lift $ try function_type) <|> try nested_union_type <|> lift inner_type
                   return $ FunctionType x y

poly_type     = do name <- (SymbolType <$> type_name) <|> (VariableType <$> try type_var)
                   whitespace1
                   let type_vars = try nested_union_type <|> lift (try rvs)
                   SimpleType . PolymorphicType name <$> type_vars `sepEndBy1` whitespace

     where rvs = record_type <|> var_type <|> symbol_type

record_type   = let key_value = (,) <$> symbol_name <* div' <*> type_definition_signature
                    div'      = spaces <* string ":" <* spaces
                    pairs     = key_value `sepEndBy` try (comma <|> not_comma)
                    inner     = RecordType . M.fromList <$> pairs 
                    inherit   = do SimpleType n <- try poly_type <|> try symbol_type <|> var_type
                                   spaces
                                   indented
                                   string "with"
                                   spaces *> indented
                                   InheritType n . M.fromList <$> pairs

                in indentPairs "{" (try inherit <|> inner) "}"

named_type = NamedType <$> name <*> option Nothing (Just <$> (try nested_union_type <|> lift inner_type))
    where name = type_var <* string ":" <* whitespace

symbol_type   = SimpleType . SymbolType <$> type_name
var_type      = SimpleType . VariableType <$> type_var

lift        :: Parser ComplexType -> Parser UnionType
lift              = fmap $ UnionType . S.fromList . (:[])
