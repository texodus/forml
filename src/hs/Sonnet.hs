{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative

import System.IO
import System.Environment

import Text.ParserCombinators.Parsec as P hiding ((<|>), many, State, spaces)
import Text.Pandoc

import qualified Data.Map as M
import Data.Char (ord, isAscii)

import Language.Javascript.JMacro



whitespace :: Parser String
whitespace = many $ oneOf "\t "

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


data Pattern = VarPattern String
             | LiteralPattern Literal
             | JSONPattern JSONLiteral
             | ListPattern
             | AliasPattern
             deriving (Show)

pattern :: Parser Pattern
pattern = undefined


-- Expressions
--------------------------------------------------------------------------------
-- ...

data Expression = ApplyExpression Expression [Expression]
                | LiteralExpression Literal
                | SymbolExpression String
                | JSExpression JExpr
                deriving (Show)

expression :: Parser Expression
expression = undefined

-- Literals
--------------------------------------------------------------------------------
-- A literal can be any

data Literal = Value ValueLiteral | JSON JSONLiteral | List ListLiteral deriving (Show)

-- ValueLiterals 

data ValueLiteral = StringLiteral String | NumLiteral Int deriving (Show)

literal :: Parser Literal
literal = try num <|> str
    where num = Value . NumLiteral . read <$> many digit
          str = Value . StringLiteral <$> (char '"' >> (anyChar `manyTill` char '"'))

-- JSON Literals

type JSONLiteral = M.Map String Expression

json_literal :: Parser JSONLiteral
json_literal = M.fromList <$> (string "{" *> spaces *> pairs <* spaces <* string "}")

    where pairs = key_value `sepEndBy` try (comma <|> not_comma)

          not_comma = whitespace >> newline >> spaces >> notFollowedBy (string "}")

          comma = spaces >> string "," >> spaces

          key_value = do key <- many (alphaNum <|> oneOf "_")
                         spaces
                         string "="
                         spaces
                         value <- expression
                         return (key, value)

-- ListLiterals

type ListLiteral = [Expression]

list_literal :: Parser ListLiteral
list_literal = undefined



-- Parsing 
--------------------------------------------------------------------------------
-- A Sonnet program is represented by a set of statements

data Statement = TypeStatement TypeDefinition TypeCase
               | DefinitionStatement String [Axiom]
               | AssertStatement Expression
               deriving (Show)

sonnetParser :: Parser [Statement]
sonnetParser  = do x <- many (statement <|> comment) <* eof
                   return $ concat x

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
           | EqualityAxiom [Pattern] Expression
           deriving (Show)

-- | TODO Should handle the case of implicit type
definition_statement :: Parser [Statement]
definition_statement = do name <- type_var
                          spaces
                          string ":"
                          spaces
                          sig <- type_signature
                          spaces
                          axioms <- try $ many equality_axiom
                          whitespace
                          return $ [DefinitionStatement name (TypeAxiom sig : axioms)]

equality_axiom :: Parser Axiom
equality_axiom = do string "|" 
                    whitespace
                    patterns <- pattern `sepBy` whitespace
                    string "="
                    spaces
                    ex <- expression
                    return $ EqualityAxiom patterns ex

-- A Statement may also be a type definition, which defines a set of values to
-- a type symbol

data TypeDefinition = TypeDefinition String [String]
                    deriving (Show)

type_statement :: Parser [Statement]
type_statement  = do whitespace
                     string "type "
                     whitespace
                     def <- type_definition
                     whitespace 
                     string "="
                     spaces
                     sig <- type_case
                     whitespace
                     return $ [TypeStatement def sig]

type_definition :: Parser TypeDefinition
type_definition = do name <- (:) <$> upper <*> many alphaNum
                     vars <- try $ do many1 $ oneOf "\t "
                                      let var = (:) <$> lower <*> many alphaNum
                                      var `sepEndBy` whitespace
                             <|> return []
                     return $ TypeDefinition name vars 

data TypeCase = UnionCase [TypeSignature] | SingleCase TypeSignature deriving (Show)

type_case :: Parser TypeCase
type_case =  (SingleCase <$> type_signature)
              



-- Type Signatures
--------------------------------------------------------------------------------
-- This is what a type signature looks like

data TypeSignature = SymbolType String
                   | VariableType String
                   | PolymorphicType TypeSignature TypeSignature
                   | JSONType (M.Map String TypeSignature)
                   | NamedType String TypeSignature
                   | FunctionType TypeSignature TypeSignature
                   deriving (Show)

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
                         vars <- inner_type `sepEndBy1` whitespace
                         return $ foldl PolymorphicType name vars

          nested_function = string "(" *> spaces *> type_signature <* spaces <* string ")"

          symbol_type = SymbolType <$> type_name

          var_type = VariableType <$> type_var

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
toEntities (c:cs)
  | isAscii c = c : toEntities cs
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


header :: String
header = "<link href='http://kevinburke.bitbucket.org/markdowncss/markdown.css' rel='stylesheet'></link>"
           ++ "<link href='lib/js/prettify.css' type='text/css' rel='stylesheet' />"
           ++ "<link href='lib/js/coda.css' type='text/css' rel='stylesheet' />"
           ++ "<script type='text/javascript' src='lib/js/prettify.js'></script>"
           ++ "<script type='text/javascript' src='lib/js/lang-hs.js'></script>"
           ++ "<script type='text/javascript' src='lib/js/jquery.js'></script>"
           ++ "<style>code{font-family:Menlo;}ul{padding:0px 48px}</style>"

footer ::String 
footer = "<script type='text/javascript'>$('code').addClass('prettyprint lang-hs');prettyPrint()</script>"
