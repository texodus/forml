{-# LANGUAGE QuasiQuotes #-}

module Sonnet.Parser(parseSonnet) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many, State)
import Language.Javascript.JMacro

import Data.Char
import Data.List

import Sonnet.AST



parseSonnet :: String -> Either ParseError [Definition]
parseSonnet src =  case parse cleanComments "Cleaning Comments" src of
  Right s -> parseSyntax s
  Left x  -> Left x
  
parseSyntax :: String -> Either ParseError [Definition]
parseSyntax code = case parse sonnetP "Parsing Syntax" code of
  Right x -> Right $ resolveScope x
  Left x  -> Left x

--------------------------------------------------------------------------------
--
-- Scope Resolution


resolveScope :: Script        -> [Definition]
resolveScope    (Script defs)  = snd $ foldl' scope (0, []) defs

  where scope list (_, s@(StructStatement (TypeStatement (Identifier i) t)))    = (fst list, Struct i t : snd list)
        scope list (_, StructStatement _)                          = error "Struct statement with implicit identifier"

        scope list (_, EmptyStatement)                             = list
        scope list (_, TypeStatement (Identifier i) t)
          | length (snd list) == 0 = (0, [Definition i [TypeAxiom t]])
        scope list (_, FunctionStatement (Identifier i) p e)
          | length (snd list) == 0 = (0, [Definition i [RelationalAxiom p e]])

        scope (i, ds) (j, TypeStatement ImplicitIdentifier t)
           = let (Definition name as) = last ds
                    in  (j, (take (length ds - 1) ds) ++ [Definition name (TypeAxiom t : as)])
        scope (i, ds) (j, TypeStatement (Identifier s) t)
                   = case last ds of
                       Definition name as | name == s ->
                         (j, (take (length ds - 1) ds) ++ [Definition name (TypeAxiom t : as)])
                       _ -> (j, ds ++ [Definition s [TypeAxiom t]])

        scope (i, ds) (j, FunctionStatement ImplicitIdentifier ps e)
          = let (Definition name as) = last ds
                     in  (j, (take (length ds - 1) ds) ++ [Definition name (RelationalAxiom ps e : as)])
        scope (i, ds) (j, FunctionStatement (Identifier s) ps e)
                   = case last ds of
                       Definition name as | name == s ->
                         (j, (take (length ds - 1) ds)
                             ++ [Definition name (RelationalAxiom ps e : as)])
                       _ -> (j, ds ++ [Definition s [RelationalAxiom ps e]])

        scope (i, ds) (j, TestStatement ImplicitIdentifier ps e)
          = let (Definition name as) = last ds
                     in  (j, (take (length ds - 1) ds) ++ [Definition name (AssertAxiom ps e : as)])
        scope (i, ds) (j, TestStatement (Identifier s) ps e)
                   = case last ds of
                       Definition name as | name == s ->
                         (j, (take (length ds - 1) ds)
                             ++ [Definition name (AssertAxiom ps e : as)])
                       _ -> (j, ds ++ [Definition s [AssertAxiom ps e]])



--------------------------------------------------------------------------------
--
-- Parser

data Statement  = TypeStatement Identifier Type
                | StructStatement Statement
                | FunctionStatement Identifier [Pattern] Expression
                | TestStatement Identifier [Pattern] Expression
                | EmptyStatement
                deriving Show

type ScriptLine = (Int, Statement)

newtype Script = Script [ScriptLine]
               deriving Show

---- Structure

cleanComments  = concat <$> many cc
  where cc      = try (comment >> return "") <|> ((:[]) <$> anyChar)
        comment = string "--" >> (anyChar `manyTill` (newline <|> (eof >> return ' ')))

sonnetP  = Script <$> (statementP `sepEndBy` newline) <* spaces <* eof

statementP = try (line structStatementP)
             <|> try (line typeStatementP)
             <|> try (line $  functionStatementP FunctionStatement '=')
             <|> try (line $ functionStatementP TestStatement '?')
             <|> line emptyExpressionP

  where line statement = (,) <$> (length <$> many (oneOf " \v\f\t")) <*> statement <* (many $ oneOf " \v\f\t")

emptyExpressionP =  (many $ oneOf " \v\f\t") >> return EmptyStatement

--- Statements

structStatementP  = StructStatement <$> (string "struct " *> spaces *> (snd <$> statementP))

functionStatementP f c = f <$> scopedId <*> option [] (try argumentsP) <* defOperator <*> expressionP

  where s            = many (oneOf " \t\v\f")
        scopedId     = option ImplicitIdentifier identifierP
        argumentsP   = s *> char '(' *> s *> argP `sepEndBy` (many (oneOf " \t\v\f,")) <* s <* char ')'
        defOperator  = s *> char c <* s
        argP         = (LiteralPattern <$> literalP) <|> ignorePatternP <|> listPatternP <|> try bindPatternP <|> varPattern

        ignorePatternP = many1 (char '_') >> return IgnorePattern
        bindPatternP   = BindPattern <$> symbolP <* char '(' <*> (argP `sepEndBy` (many (oneOf " \t\v\f,"))) <* char ')'
        varPattern     = VarPattern <$> symbolP
        listPatternP = makePattern <$> (char '[' *> spaces *> (argP `sepEndBy` (many (oneOf " \t\v\f,"))) <* char ']')

        makePattern [] = BindPattern "nil" []
        makePattern (x:xs) = BindPattern "cons" [x, (makePattern xs)]


typeStatementP  = TypeStatement <$> identifierP <* typeOperatorP <*> typeP

  where typeOperatorP  = spaces *> string ":" *> spaces >> return ()
        typeP          = try funTypeP <|> nestedTypeP <|> try polyTypeP <|> typeVarP <|> monoTypeP
        nestedTypeP    = char '(' >> spaces >> typeP <* spaces <* char ')'
        funTypeP       = FunType <$> (nestedTypeP <|> try polyTypeP <|> typeVarP <|> monoTypeP) <* spaces <* string "->" <* spaces <*> typeP
        monoTypeP      = Type <$> typeSymbolP
        polyTypeP      = PolymorphicType <$> typeSymbolP <* char '(' <* spaces <*> (typeP `sepEndBy1` spaces) <* spaces <* char ')'
        typeVarP       = TypeVar <$> typeVarSymbolP
        typeSymbolP    = (:) <$> upper <*> many (letter <|> oneOf "_'")
        typeVarSymbolP = (:[]) <$> lower :: Parser String

-- Expressions

expressionP = do exp <- nestedExpressionP
                       <|> jsExpressionP
                       <|> listExpressionP
                       <|> try ifExpressionP
               --      <|> try letExpressionP
                       <|> try prefixExpressionP
                       <|> symbolExpressionP
                       <|> literalExpressionP
                 try (infixTail exp) <|> return exp

  where operatorP = Operator <$> many1 (oneOf "><-*^%$|+/?#")
        infixTail exp = do op <- spaces *> operatorP <* spaces
                           exp2 <- expressionP
                           return (InfixExpression exp op exp2)

nestedExpressionP = char '(' *> spaces *> expressionP <* spaces <* char ')'

jsExpressionP = JSExpression <$> (char '`' *> ((convert . either (error "Failed parsing javascript") id . parseJM) <$> (anyChar `manyTill` (char '`'))))
  where convert expr = [$jmacroE| (function(x) { var !cxt = x; var !ans; `(expr)`; return ans; }) |]

listExpressionP = makeList <$> (char '[' *> spaces *> (expressionP `sepEndBy` (many (oneOf " \t\v\f\r\n,"))) <* char ']')
  where makeList [] = PrefixExpression "nil" []
        makeList (x:xs) = PrefixExpression "cons" [x, (makeList xs)]

ifExpressionP = IfExpression <$> (string "if" *> spaces *> expressionP)
                <* spaces <* string "then" <* spaces <*> expressionP
                <* spaces <* string "else" <* spaces <*> expressionP

letExpressionP = error "Let expression not yet implemented" 
                 -- LetExpression <$> (string "let" *> spaces *> (try (functionStatementP FunctionStatement '=') `sepEndBy` spaces))
                 -- <*  spaces <* string "in" <* spaces <*> expressionP

symbolExpressionP = PrefixExpression <$> symbolP <*> return []

prefixExpressionP = PrefixExpression <$> symbolP <* char '(' <* spaces <*> expressionP `sepBy` (many (oneOf " \t\v\f\r\n,")) <* char ')'

literalExpressionP = LiteralExpression <$> literalP

literalP = stringP <|> booleanP <|> numP
  where stringP  = StringLiteral <$> (char '"' *> manyTill anyChar (char '"'))
        booleanP = BooleanLiteral <$> ((string "True" >> return True) <|> (string "False" >> return False))
        numP     = (NumLiteral . read) <$> ((++) <$> many1 digit <*> (radix <|> return ""))
        radix    = (++) <$> string "." <*> many1 digit

identifierP = Identifier <$> symbolP

symbolP = (:) <$> letter <*> many (alphaNum <|> oneOf "_'")

