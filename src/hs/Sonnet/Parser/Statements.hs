{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Sonnet.Parser.Statements where

import Language.Javascript.JMacro

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (lift)

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import Text.Parsec.Indent  hiding (same)
import Text.Parsec.Expr

import System.IO.Unsafe

import qualified Data.Map as M

import Sonnet.Parser.Utils
import Sonnet.Parser.Types
import Sonnet.Parser.AST

-- Statements
-- -----------------------------------------------------------------------------
-- A Statement may be a definition, which is a list of axioms associated with a
-- symbol


definition_statement :: Parser [Definition]
definition_statement = do whitespace
                          name <- try symbol_name <|> (Symbol <$> many1 (char '_'))
                          sig <- first
                          eqs <- (try $ spaces *> (withPos . many . try $ eq_axiom)) <|> return []
                          whitespace
                          if length sig == 0 && length eqs == 0 
                             then parserFail "Definition Axioms"
                             else return $ [Definition name (sig ++ eqs)]

    where first = try type_or_first
                  <|> ((:[]) <$> try naked_eq_axiom) 
                  <|> return []

          type_or_first = (:) <$> type_axiom <*> second

          second = option [] ((:[]) <$> try (no_args_eq_axiom (Match [] Nothing)))

          eq_axiom   = do try (spaces >> same) <|> (whitespace >> return ())
                          string "|"
                          naked_eq_axiom

          naked_eq_axiom = do whitespace
                              patterns <- match
                              no_args_eq_axiom patterns

          no_args_eq_axiom patterns = do whitespace *> string "=" *> spaces *> indented
                                         ex <- withPos expression
                                         return $ EqualityAxiom patterns ex

          type_axiom = do spaces
                          indented
                          string ":"
                          spaces
                          indented
                          TypeAxiom <$> withPos type_axiom_signature

expression_statement :: Parser [Statement]
expression_statement = do whitespace
                          (:[]) . ExpressionStatement <$> withPos expression

-- Type definitions

type_definition :: Parser TypeDefinition
type_statement  :: Parser [Statement]

type_statement  = do whitespace
                     string "type"
                     whitespace1
                     def <- type_definition
                     whitespace 
                     string "="
                     spaces
                     sig <- withPos $ do option "" (string "|" <* spaces)
                                         type_definition_signature
                     whitespace
                     return $ [TypeStatement def sig]

type_definition = do name <- (:) <$> upper <*> many alphaNum
                     vars <- try vars' <|> return []
                     return $ TypeDefinition name vars

    where vars' = do many1 $ oneOf "\t "
                     let var = (:) <$> lower <*> many alphaNum
                     var `sepEndBy` whitespace



-- Patterns
-- -----------------------------------------------------------------------------
-- TODO when patterns

match :: Parser Match
match = try conditional <|> ((\x -> Match x Nothing) <$> (pattern `sepEndBy` whitespace1))

    where conditional = do x <- try pattern `sepEndBy` try whitespace1
                           string "when"
                           spaces
                           indented
                           ex <- withPos expression
                           spaces
                           indented
                           return $ Match x (Just ex)

pattern             :: Parser Pattern
var_pattern         :: Parser Pattern
literal_pattern     :: Parser Pattern
record_pattern      :: Parser Pattern
list_pattern        :: Parser Pattern
any_pattern         :: Parser Pattern
naked_apply_pattern :: Parser Pattern
apply_pattern       :: Parser Pattern
view_pattern        :: Parser Pattern

pattern = try literal_pattern
          <|> try naked_apply_pattern
          <|> try var_pattern
          <|> any_pattern
          <|> record_pattern
          <|> list_pattern
          <|> indentPairs "(" (try view_pattern <|> try apply_pattern <|> pattern) ")"

view_pattern        = ViewPattern <$> expression <* spaces <* string "->" <* whitespace <*> pattern 
var_pattern         = VarPattern <$> type_var
literal_pattern     = LiteralPattern <$> literal          
any_pattern         = many1 (string "_") *> return AnyPattern
naked_apply_pattern = NamedPattern <$> many1 letter <* string ":" <*> return Nothing
apply_pattern       = NamedPattern 
                      <$> many1 letter 
                      <* string ":" 
                      <*> (Just <$> (whitespace *> pattern))

record_pattern  = RecordPattern . M.fromList <$> indentPairs "{" pairs' "}"

    where pairs' = key_eq_val `sepEndBy` try (comma <|> not_comma)
          key_eq_val = do key <- symbol_name --many (alphaNum <|> oneOf "_")
                          spaces
                          string "=" <|> string ":"
                          spaces
                          value <- pattern
                          return (key, value)

list_pattern = ListPattern <$> indentPairs "[" (pattern `sepBy` try comma) "]"



-- Expressions
-- -----------------------------------------------------------------------------
-- TODO nested record accessors
-- TODO recursive applyexpression
-- TODO do expressions

expression          :: Parser Expression
other_expression    :: Parser Expression
let_expression      :: Parser Expression
do_expression       :: Parser Expression
apply_expression    :: Parser Expression
infix_expression    :: Parser Expression
named_expression    :: Parser Expression
function_expression :: Parser Expression
js_expression       :: Parser Expression
record_expression   :: Parser Expression
literal_expression  :: Parser Expression
symbol_expression   :: Parser Expression
accessor_expression :: Parser Expression
list_expression     :: Parser Expression
if_expression       :: Parser Expression

expression = try if_expression <|> try infix_expression <|> other_expression

other_expression = try let_expression
                    <|> try do_expression
                    <|> try named_expression
                    <|> try apply_expression
                    <|> function_expression
                    <|> try accessor_expression
                    <|> inner_expression

inner_expression :: Parser Expression
inner_expression = indentPairs "(" expression ")" 
                   <|> js_expression 
                   <|> record_expression 
                   <|> literal_expression
                   <|> try accessor_expression
                   <|> symbol_expression
                   <|> list_expression

let_expression = withPosTemp $ do string "let"
                                  whitespace1
                                  defs <- concat <$> withPos def
                                  spaces
                                  same
                                  LetExpression <$> return defs <*> expression

    where def = try definition_statement `sepBy1` try (spaces *> same)

do_expression  = do string "do"
                    whitespace1
                    withPos $ try bind_expression <|> try return_expression

    where bind_expression = do p <- pattern
                               whitespace <* (string "<-" <|> string "←") <* whitespace 
                               ex <- withPos expression 
                               spaces *> same
                               f ex p <$> (try bind_expression <|> try return_expression)

          return_expression = do v <- expression
                                 option v $ try $ unit_bind v

          unit_bind v = do spaces *> same
                           f v AnyPattern <$> (try bind_expression <|> try return_expression)

          f ex pat zx=  ApplyExpression 
                           (SymbolExpression (Operator ">>="))
                           [ ex, (FunctionExpression 
                                      [ EqualityAxiom 
                                        (Match [pat] Nothing)
                                        zx ]) ]



if_expression = withPos $ do string "if"
                             whitespace1
                             e <- try infix_expression <|> other_expression
                             spaces
                             string "then"
                             whitespace1
                             t <- try infix_expression <|> other_expression
                             spaces
                             string "else"
                             whitespace1
                             IfExpression e t <$> (try infix_expression <|> other_expression) 

infix_expression = buildExpressionParser table term 

    where table  = [ [ix "^"]
                   , [ix "*", ix "/"]
                   , [ix "+", ix "-"]
                   , [ Infix user_op_right AssocRight, Infix user_op_left AssocLeft ]
                   , [ix "<", ix "<=", ix ">=", ix ">", ix "==", ix "!="]
                   , [px "not"]
                   , [ix "&&", ix "||", ix "and", ix "or" ] ]

          ix s   = Infix (try . op $ (Operator <$> string s) <* notFollowedBy operator) AssocLeft
          px s   = Prefix $ try (whitespace >> string s >> return (ApplyExpression (SymbolExpression (Operator s)) . (:[])))
          term   = try other_expression
          
          end (reverse -> x : xs) = x : reverse xs

          user_op_left = try $ do spaces
                                  op' <- not_system $ not_reserved (many1 operator) 
                                  spaces
                                  return (\x y -> ApplyExpression (SymbolExpression (Operator op')) [x, y])

          user_op_right = try $ do spaces
                                   op' @ (end -> x : _) <- not_system $ not_reserved (many1 operator)
                                   spaces
                                   if x == ':'
                                       then return (\x y -> ApplyExpression (SymbolExpression (Operator op')) [x, y])
                                       else parserFail "Operator"
          op p   = do spaces
                      op' <- SymbolExpression <$> p
                      spaces
                            
                      return (\x y -> ApplyExpression op' [x, y])

named_expression = NamedExpression 
                   <$> (symbol_name <* string ":") 
                   <*> option Nothing (Just <$> try (spaces *> indented *> other_expression))

accessor_expression = do x <- indentPairs "(" expression ")" 
                              <|> js_expression 
                              <|> record_expression 
                              <|> literal_expression
                              <|> symbol_expression
                              <|> list_expression

                         string "."
                         z <- symbol_name --type_var
                         return $ ApplyExpression 
                                    (FunctionExpression 
                                         [ EqualityAxiom 
                                           (Match [RecordPattern (M.fromList [(z, VarPattern "x")])] Nothing)
                                           (SymbolExpression (Symbol "x")) ] )
                                    [x]

apply_expression = ApplyExpression <$> inner_expression <*> (many1 . try $ whitespace *> inner_expression)

withPosTemp p = do x <- get
                   try p <|> (put x >> parserFail "mjsakdfjsnkdn")

function_expression = withPosTemp $ do string "\\" <|> string "λ" <|> string "\955"
                                       whitespace
                                       t <- option [] (try $ ((:[]) <$> type_axiom <* spaces))
                                       eqs <- try eq_axiom `sepBy1` try (spaces *> string "|" <* whitespace)
                                       return $ FunctionExpression (t ++ eqs)

    where type_axiom = do string ":"
                          spaces
                          indented
                          TypeAxiom <$> withPos type_axiom_signature

          eq_axiom   = do patterns <- match
                          string "="
                          spaces
                          indented
                          ex <- withPos expression
                          return $ EqualityAxiom patterns ex

-- TODO allow ` escaping
-- TODO it would be nice if we parsed javascript too ...
end (reverse -> x : xs) = x : reverse xs


js_expression = JSExpression <$> join (p <$> indentPairs "`" (many $ noneOf "`") "`")
    where p (parseJM . wrap -> Right (BlockStat [AssignStat _ x])) = return x
          p y @ (parseJM . wrap -> Left x)  =
              case parseJM y of
                Left ex -> parserFail "Javascript"
                Right x -> return [jmacroE| (function() { `(x)`; })() |] 
          
          wrap x = "__ans__ = " ++ x ++ ";"

record_expression = indentPairs "{" (try inherit <|> (RecordExpression . M.fromList <$>  pairs')) "}"
    where pairs' = withPos $ (try key_eq_val <|> try function) `sepBy` try (try comma <|> not_comma)

          function = do n <- symbol_name 
                        whitespace
                        eqs <- try eq_axiom `sepBy1` try (spaces *> string "|" <* whitespace)
                        return $ (n, FunctionExpression eqs)

          eq_axiom   = do patterns <- match
                          string "="
                          spaces
                          indented
                          ex <- withPos expression
                          return $ EqualityAxiom patterns ex

          inherit = do ex <- expression
                       spaces *> indented
                       string "with"
                       spaces *> indented
                       ps <- pairs'
                       return $ InheritExpression ex (M.fromList ps)

          key_eq_val = do key <- symbol_name
                          whitespace
                          string "=" <|> string ":"
                          spaces
                          value <- withPos expression
                          return (key, value)

literal_expression = LiteralExpression <$> literal
symbol_expression  = SymbolExpression <$> symbol_name
list_expression    = ListExpression <$> indentPairs "[" (expression `sepBy` comma) "]"



-- Literals
-- -----------------------------------------------------------------------------
-- Literals in Sonnet are limited to strings and numbers - 

-- TODO string escaping
-- TODO heredoc
-- TODO string interpolation
literal :: Parser Literal
literal = try flt <|> try num <|> try str
    where flt = DoubleLiteral . read <$> do x <- many1 digit 
                                            string "."
                                            y <- many1 digit
                                            return $ x ++ "." ++ y
          num = IntLiteral . read <$> many1 digit
          str = StringLiteral <$> (char '"' >> (anyChar `manyTill` char '"'))

