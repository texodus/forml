{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative
import Control.Monad.State

import System.IO
import System.Environment

import Text.Parsec as P hiding ((<|>), many, State, spaces, parse)
import Text.Parsec.Indent
import Text.Pandoc

import qualified Data.Map as M
import qualified Data.List as L

import Data.Char (ord, isAscii)

-- import Language.Javascript.JMacro



-- Structure & comments
--------------------------------------------------------------------------------
-- ...

type Parser a = ParsecT String () (State SourcePos) a

parse :: Parser a -> SourceName -> String -> Either ParseError a
parse parser sname input = runIndent sname $ runParserT parser () sname input

whitespace :: Parser String
whitespace = many $ oneOf "\t "



-- A special implementation of the spaces parser from Parsec, which also treats
-- comments as whitespace

spaces :: Parser ()
spaces = try inter_comments <|> skipMany (oneOf "\t ")
    where inter_comments = do finish_line 
                              comment `manyTill` try line_start
                              return ()
          finish_line = oneOf "\t " `manyTill` newline
          line_start = do count 4 (oneOf "\t ")
                          skipMany (oneOf "\t ")




-- Patterns
--------------------------------------------------------------------------------
-- ...


-- data Pattern = VarPattern String
--              | LiteralPattern Literal
--              | AliasPattern
--              deriving (Show)

-- pattern :: Parser Pattern
-- pattern = undefined


-- -- Expressions
-- --------------------------------------------------------------------------------
-- -- ...

-- data Expression = ApplyExpression Expression [Expression]
--                 | LiteralExpression Literal
--                 | SymbolExpression String
--                 | JSExpression JExpr
--                 deriving (Show)

-- expression :: Parser Expression
-- expression = undefined

-- -- Literals
-- --------------------------------------------------------------------------------
-- -- Literals in Sonnet are limited to strings and numbers - 

-- -- 
-- data Literal = StringLiteral String | NumLiteral Int | JSON (M.Map String Expression) | List [Expression] 

-- instance Show Literal where
--    show (JSON x)  = show x
--    show (List x)  = show x

-- -- ValueLiterals 


-- literal :: Parser Literal
-- literal = try num <|> str
--     where num = NumLiteral . read <$> many digit
--           str = StringLiteral <$> (char '"' >> (anyChar `manyTill` char '"'))

-- -- JSON Literals


-- json_literal :: Parser Literal
-- json_literal = JSON . M.fromList <$> (string "{" *> spaces *> pairs <* spaces <* string "}")

--     where pairs = key_value `sepEndBy` try (comma <|> not_comma)

--           not_comma = whitespace >> newline >> spaces >> notFollowedBy (string "}")

--           comma = spaces >> string "," >> spaces

--           key_value = do key <- many (alphaNum <|> oneOf "_")
--                          spaces
--                          string "="
--                          spaces
--                          value <- expression
--                          return (key, value)

-- -- ListLiterals

-- list_literal :: Parser Literal
-- list_literal = undefined



-- Parsing 
--------------------------------------------------------------------------------
-- A Sonnet program is represented by a set of statements

data Statement = TypeStatement TypeDefinition TypeSignature
               | DefinitionStatement String [Axiom]
              -- | ExpressionStatement Expression

instance Show Statement where
    show (TypeStatement t c) = "type " ++ show t ++ " = " ++ show c ++ "\n"
    show (DefinitionStatement s as) = let f = concat . fmap show in s ++ f as ++ "\n"

newtype Program = Program [Statement]

instance Show Program where
     show (Program ss) = concat . fmap show $ ss

sonnetParser :: Parser Program
sonnetParser  = do x <- many (statement <|> comment) <* eof
                   return $ Program $ concat x

    where statement = count 4 (oneOf "\t ") >> whitespace >> statement' <* newline
          statement' = try type_statement <|> definition_statement

comment :: forall a. Parser [a]
comment = do anyChar `manyTill` newline
             return []



-- Statements
--------------------------------------------------------------------------------
-- A Statement may be a definition, which is a list of axioms associated with a
-- symbol

data Axiom = TypeAxiom TypeSignature
         --  | EqualityAxiom [Pattern] Expression

instance Show Axiom where
    show (TypeAxiom x) = ":" ++ show x
    show _ = undefined

-- | TODO Should handle the case of implicit type
definition_statement :: Parser [Statement]
definition_statement = do name <- type_var
                          spaces
                          string ":"
                          spaces
                          sig <- type_signature
                          spaces
                          axioms <- return [] -- try $ many equality_axiom
                          whitespace
                          return $ [DefinitionStatement name (TypeAxiom sig : axioms)]

-- equality_axiom :: Parser Axiom
-- equality_axiom = do string "|" 
--                     whitespace
--                     patterns <- pattern `sepBy` whitespace
--                     string "="
--                     spaces
--                     ex <- expression
--                     return $ EqualityAxiom patterns ex

-- A Statement may also be a type definition, which defines a set of values to
-- a type symbol

data TypeDefinition = TypeDefinition String [String]

instance Show TypeDefinition where
    show (TypeDefinition name vars) = concat . L.intersperse " " $ name : vars

type_statement :: Parser [Statement]
type_statement  = do whitespace
                     string "type "
                     whitespace
                     def <- type_definition
                     whitespace 
                     string "="
                     spaces
                     sig <- type_signature
                     whitespace
                     return $ [TypeStatement def sig]

type_definition :: Parser TypeDefinition
type_definition = do name <- (:) <$> upper <*> many alphaNum
                     vars <- try $ do many1 $ oneOf "\t "
                                      let var = (:) <$> lower <*> many alphaNum
                                      var `sepEndBy` whitespace
                             <|> return []
                     return $ TypeDefinition name vars 
              



-- Type Signatures
--------------------------------------------------------------------------------
-- This is what a type signature looks like

data TypeSignature = PolymorphicType SimpleTypeSignature [TypeSignature]
                   | JSONType (M.Map String TypeSignature)
                   | NamedType String TypeSignature
                   | FunctionType TypeSignature TypeSignature
                   | SimpleType SimpleTypeSignature

data SimpleTypeSignature = SymbolType String | VariableType String

instance Show TypeSignature where
    show (SimpleType y) = show y
    show (PolymorphicType x y) = "(" ++ show x ++ " " ++ (concat $ L.intersperse " " $ fmap show y) ++ ")"
    show (NamedType name t) = name ++ " " ++ show t
    show (FunctionType g@(FunctionType _ _) h) = "(" ++ show g ++ ")" ++ " -> " ++ show h
    show (FunctionType g h) = show g ++ " -> " ++ show h
    show (JSONType m) = "{ " ++ (g $ M.mapWithKey f m) ++ " }"
        where f k v = k ++ ": " ++ show v
              g = concat . L.intersperse ", " . fmap (\ (_, x) -> x) . M.toAscList

instance Show SimpleTypeSignature where
    show (SymbolType x) = x
    show (VariableType x) = x

type_signature :: Parser TypeSignature
type_signature = try function_type <|> inner_type

    where function_type = do x <- inner_type
                             spaces
                             (string "->" <|> string "→")
                             spaces
                             y <- type_signature
                             return $ FunctionType x y

          inner_type = nested_function <|> record_type <|> try poly_type <|> var_type <|> symbol_type

          poly_type = do name <- (SymbolType <$> type_name) <|> (VariableType <$> type_var)
                         oneOf "\t "
                         whitespace
                         let type_vars = nested_function <|> record_type <|> var_type <|> symbol_type
                         vars <- type_vars `sepEndBy1` whitespace
                         return $ PolymorphicType name vars

          nested_function = string "(" *> spaces *> type_signature <* spaces <* string ")"

          symbol_type = SimpleType . SymbolType <$> type_name

          var_type = SimpleType . VariableType <$> type_var

          record_type = (JSONType . M.fromList) <$> (string "{" *> spaces *> pairs <* spaces <* string "}")

          pairs = key_value `sepEndBy` try (comma <|> not_comma)

          not_comma = whitespace >> newline >> spaces >> notFollowedBy (string "}")

          comma = spaces >> string "," >> spaces

          key_value = (,) <$> many (alphaNum <|> oneOf "_") <* spaces <* string ":" <* spaces <*> type_signature

type_name :: Parser String
type_name = (:) <$> upper <*> many alphaNum  

type_var :: Parser String
type_var = (:) <$> lower <*> many alphaNum  


          

--------------------------------------------------------------------------------
--
-- Main

toEntities :: String -> String
toEntities [] = ""
toEntities (c:cs) | isAscii c = c : toEntities cs
                  | otherwise = "&#" ++ show (ord c) ++ ";" ++ toEntities cs

toHTML :: String -> String
toHTML = toEntities . writeHtmlString defaultWriterOptions . readMarkdown defaultParserState

main :: IO ()
main  = do args <- getArgs
           let name = head args
           hFile  <- openFile name ReadMode
           src    <- (\ x -> x ++ "\n") <$> hGetContents hFile
           writeFile "prelude.html" $ toHTML (header ++ src ++ footer)
           putStrLn $ concat $ take 80 $ repeat "-"
           case parse sonnetParser "Parsing" src of
             Left ex -> do putStrLn $ show ex
             Right x -> do putStrLn $ show x
                           putStrLn $ "success"

    where header = "<link href='http://kevinburke.bitbucket.org/markdowncss/markdown.css' rel='stylesheet'>"
                        ++ "</link>"             
                        ++ "<link href='lib/js/prettify.css' type='text/css' rel='stylesheet' />"
                        ++ "<link href='lib/js/coda.css' type='text/css' rel='stylesheet' />"
                        ++ "<script type='text/javascript' src='lib/js/prettify.js'></script>"
                        ++ "<script type='text/javascript' src='lib/js/lang-hs.js'></script>"
                        ++ "<script type='text/javascript' src='lib/js/jquery.js'></script>"

          footer = "<script type='text/javascript'>$('code').addClass('prettyprint lang-hs');"
                        ++ "prettyPrint()</script>"
