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
{-# LANGUAGE ScopedTypeVariables #-}

module Formal.Types.Expression where

import Text.InterpolatedString.Perl6
import Language.Javascript.JMacro

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (lift)

import Text.Parsec         hiding ((<|>), State, many, spaces, parse, label)
import Text.Parsec.Indent  hiding (same)
import Text.Parsec.Expr

import qualified Data.Map as M
import qualified Data.List as L

import Formal.Parser.Utils
import Formal.Javascript.Utils

import Formal.Types.Literal
import Formal.Types.Type
import Formal.Types.Symbol
import Formal.Types.Pattern
import Formal.Types.Axiom

import Data.String.Utils hiding (join)
import Data.Monoid

import Prelude hiding (curry, (++))



-- Expression
-- --------------------------------------------------------------------------------

class ToLocalStat a where
    toLocal :: a -> JStat

data Lazy = Once | Every

data Expression d = ApplyExpression (Expression d) [Expression d]
                  | IfExpression (Expression d) (Expression d) (Expression d)
                  | LiteralExpression Literal
                  | SymbolExpression Symbol
                  | JSExpression JExpr
                  | LazyExpression (Addr (Expression d)) Lazy
                  | FunctionExpression [Axiom (Expression d)]
                  | RecordExpression (M.Map Symbol (Expression d))
                  | InheritExpression (Expression d) (M.Map Symbol (Expression d))
                  | LetExpression [d] (Expression d)
                  | ListExpression [Expression d]
                  | AccessorExpression (Expression d) [Symbol]

instance (Show d) => Show (Expression d) where

    show (ApplyExpression x @ (SymbolExpression (show -> f : _)) y) 
        | f `elem` "abcdefghijklmnopqrstuvwxyz" = [qq|$x {sep_with " " y}|]
        | length y == 2                         = [qq|{y !! 0} $x {y !! 1}|]

    show (ApplyExpression x y)   = [qq|$x {sep_with " " y}|]
    show (IfExpression a b c)    = [qq|if $a then $b else $c|]
    show (LiteralExpression x)   = show x
    show (SymbolExpression x)    = show x
    show (ListExpression x)      = [qq|[ {sep_with ", " x} ]|]
    show (FunctionExpression as) = replace "\n |" "\n     |" $ [qq|(λ{sep_with "| " as})|]
    show (JSExpression x)        = "`" ++ show (renderJs x) ++ "`"
    show (LazyExpression x Once)      = "lazy " ++ show x
    show (LazyExpression x Every)      = "do " ++ show x
    show (LetExpression ax e)    = replace "\n |" "\n     |" $ [qq|let {sep_with "\\n| " ax} in ($e)|]
    show (RecordExpression m)    = [qq|\{ {unsep_with " = " m} \}|] 
    show (InheritExpression x m) = [qq|\{ $x with {unsep_with " = " m} \}|] 


instance (Syntax d) => Syntax (Expression d) where

    syntax = try if' <|> try infix' <|> other

        where other = try let'
                      <|> try do'
                      <|> try lazy
                      <|> try named
                      <|> try apply
                      <|> function
                      <|> try accessor
                      <|> inner

              inner = indentPairs "(" syntax ")" 
                      <|> js 
                      <|> record 
                      <|> literal
                      <|> try accessor
                      <|> symbol
                      <|> try array
                      <|> list

              let' = withPosTemp $ do string "let"
                                      whitespace1
                                      defs <- withPos def
                                      spaces
                                      same
                                      LetExpression <$> return defs <*> syntax

                  where def = try syntax `sepBy1` try (spaces *> same)

              do'  = do s <- getPosition
                        string "do"
                        i <- try (string "!") <|> return ""
                        whitespace1
                        l <- withPos line
                        if i == "!" 
                           then return$ ApplyExpression (SymbolExpression (Symbol "run")) [wrap s l]
                           else return$ wrap s l

                  where line = try bind <|> try let_bind <|> try return'

                        wrap s x = LazyExpression (Addr s s (ApplyExpression (SymbolExpression (Symbol "run")) [x])) Every

                        bind = do p <- syntax
                                  whitespace <* (string "<-" <|> string "←") <* whitespace 
                                  ex <- withPos syntax 
                                  spaces *> same
                                  f ex p <$> addr line

                        let_bind = withPosTemp $ do string "let"
                                                    whitespace1
                                                    defs <- withPos def
                                                    spaces
                                                    same
                                                    LetExpression <$> return defs <*> line

                            where def = try syntax `sepBy1` try (spaces *> same)

                        return' = do v <- syntax
                                     option v $ try $ unit_bind v

                        unit_bind v = do spaces *> same
                                         f v AnyPattern <$> addr line

                        f ex pat zx = ApplyExpression 
                                         (SymbolExpression (Operator ">>="))
                                         [ ex, (FunctionExpression 
                                                    [ EqualityAxiom 
                                                      (Match [pat] Nothing)
                                                      zx ]) ]

              lazy  = do string "lazy"
                         whitespace1
                         LazyExpression <$> withPos (addr$ try syntax) <*> return Once

              if' = withPos $ do string "if"
                                 whitespace1
                                 e <- try infix' <|> other
                                 spaces
                                 string "then"
                                 whitespace1
                                 t <- try infix' <|> other
                                 spaces
                                 string "else"
                                 whitespace1
                                 IfExpression e t <$> (try infix' <|> other) 

              infix' = buildExpressionParser table term 

                  where table  = [ [ix "^"]
                                 , [ix "*", ix "/"]
                                 , [px "-" ]
                                 , [ix "+", ix "-"]
                                 , [ Infix user_op_right AssocRight, Infix user_op_left AssocLeft ]
                                 , [ix "<", ix "<=", ix ">=", ix ">", ix "==", ix "!="]
                                 , [ix "&&", ix "||", ix "and", ix "or" ] ]

                        ix s   = Infix (try . op $ (Operator <$> string s) <* notFollowedBy operator) AssocLeft

                        px s   = Prefix (try neg)
                                 where neg = do spaces
                                                op <- SymbolExpression . Operator <$> string s
                                                spaces
                                                return (\x -> ApplyExpression op [x])
                                 
                        term   = try other
                        
                        user_op_left = try $ do spaces
                                                op' <- not_system $ not_reserved (many1 operator) 
                                                spaces
                                                return $ f op'

                        user_op_right = try $ do spaces
                                                 op' @ (end -> x : _) <- g operator
                                                 spaces
                                                 if x == ':'
                                                     then return $ f op'
                                                     else parserFail "Operator"

                        f op' x y = ApplyExpression (SymbolExpression (Operator op')) [x, y]

                        g = not_system . not_reserved . many1

                        op p   = do spaces
                                    op' <- SymbolExpression <$> p
                                    spaces
                                          
                                    return (\x y -> ApplyExpression op' [x, y])

              named_key = do x <- syntax
                             char ':'
                             return $ RecordExpression (M.fromList [(x, SymbolExpression (Symbol "true"))]) 

              named = do x @ (RecordExpression (M.toList -> (k, _): _)) <- named_key
                         option x $ try $ do whitespace
                                             z <- other
                                             return $ RecordExpression (M.fromList [(k, z)])

              accessor = do s <- getPosition
                            x <- indentPairs "(" syntax ")" 
                                 <|> js 
                                 <|> record 
                                 <|> literal
                                 <|> symbol
                                 <|> list

                            string "."
                            z <- syntax
                            f <- getPosition
                            return $ acc_exp (Addr s f) x z

              -- TODO this is nasty & may trip up closure, please fix
              acc_exp f x z = ApplyExpression 
                              (FunctionExpression 
                               [ EqualityAxiom 
                                 (Match [RecordPattern (M.fromList [(z, VarPattern "__x__")]) Partial] Nothing)
                                 (f (SymbolExpression (Symbol "__x__"))) ] )
                              [x]

              apply = ApplyExpression <$> inner <*>  (try cont <|> halt)

                  where cont = do x <- whitespace *> (try named_key <|> inner)
                                  option [x] ((x:) <$> try (whitespace *> (try cont <|> halt)))

                        halt = (:[]) <$> (whitespace *> (try let'
                                          <|> try do'
                                          <|> try lazy
                                          <|> function))
                                  


              withPosTemp p = do x <- get
                                 try p <|> (put x >> parserFail ("Indented to exactly" ++ show x))

              function = withPosTemp $ do try (char '\\') <|> char 'λ'
                                          whitespace
                                          t <- option [] (try $ ((:[]) <$> type_axiom <* spaces))
                                          eqs <- try eq_axiom `sepBy1` try (spaces *> string "|" <* whitespace)
                                          return $ FunctionExpression (t ++ eqs)

                  where type_axiom = do string ":"
                                        spaces
                                        indented
                                        TypeAxiom <$> withPos type_axiom_signature

                        eq_axiom   = do patterns <- syntax
                                        string "="
                                        spaces
                                        indented
                                        ex <- withPos (addr syntax)
                                        return $ EqualityAxiom patterns ex

              js = g <$> indentPairs "`" (many $ noneOf "`") "`"
                  where p (parseJM . wrap -> Right (BlockStat [AssignStat _ x])) =
                            [jmacroE| (function() { return `(x)`; }) |]
                        -- p (parseJM -> Right z) = 
                        --     case z of
                        --       (BlockStat z @ (last -> ApplStat x y)) ->
                        --           [jmacroE| (function() {
                        --                         `(drop (length z - 1) z)`;
                        --                         return `(ApplExpr x y)`;
                        --                      }) |]
                        --       z -> [jmacroE| (function() { `(z)`; }) |] 
                        p (parseJM . (++";") -> Right z) = [jmacroE| (function() { `(z)`; }) |]
                        p (parseJM . concat . L.intersperse ";" . h . split ";" -> Right x) = 
                            [jmacroE| (function() { `(x)`; }) |]
                        p _ = error "\n\nJavascript parsing failed"
                        
                        g x | last x == '!' = ApplyExpression (SymbolExpression (Symbol "run"))
                                              [JSExpression (p (take (length x - 1) x))]
                            | otherwise = JSExpression$ p x

                        h [] = []
                        h (x:[]) = ["return " ++ x]
                        h (x:xs) = x : h xs
 
                        wrap x = "__ans__ = " ++ x ++ ";"

              record = indentPairs "{" (try inherit <|> (RecordExpression . M.fromList <$>  pairs')) "}"
        
                  where pairs' = withPos $ (try key_eq_val <|> try function') 
                                         `sepBy` try (try comma <|> not_comma)

                        function' = do n <- syntax 
                                       whitespace
                                       eqs <- try eq_axiom `sepBy1` try (spaces *> string "|" <* whitespace)
                                       return $ (n, FunctionExpression eqs)

                        eq_axiom   = do patterns <- syntax
                                        string "="
                                        spaces
                                        indented
                                        ex <- withPos (addr syntax)
                                        return $ EqualityAxiom patterns ex

                        inherit = do ex <- syntax
                                     spaces *> indented
                                     string "with"
                                     spaces *> indented
                                     ps <- pairs'
                                     return $ InheritExpression ex (M.fromList ps)

                        key_eq_val = do key <- syntax
                                        whitespace
                                        string "=" <|> string ":"
                                        spaces
                                        value <- withPos syntax
                                        return (key, value)

              literal = undo <$> syntax

              undo (StringLiteral x) = to_escaped . split "`" $ x
              undo l = LiteralExpression l

              to_escaped (x:s:y:xs) =
                  ApplyExpression (SymbolExpression$ Operator "+++") 
                                      [ ApplyExpression (SymbolExpression$ Operator "+++") 
                                        [ ApplyExpression (SymbolExpression$ Operator "+++")
                                          [LiteralExpression $ StringLiteral x
                                          , p s ]
                                        , LiteralExpression $ StringLiteral y]
                                      , to_escaped xs ]
              to_escaped (l:[]) = LiteralExpression $ StringLiteral l
              to_escaped [] =  LiteralExpression $ StringLiteral ""

              p s = case parse syntax "Escaped String" s of
                      Left x -> error$ show x
                      Right x -> x

              symbol  = SymbolExpression <$> syntax

              list    = ListExpression <$> indentPairs "[" v "]"
                  where v = do whitespace
                               withPos (syntax `sepBy` try (try comma <|> not_comma))

              array   = f <$> indentAsymmetricPairs "[:" v (try (string ":]") <|> string "]")

                  where v = do whitespace
                               withPos (syntax `sepBy` try (try comma <|> not_comma))

                        f [] = RecordExpression (M.fromList [(Symbol "nil", SymbolExpression (Symbol "true"))])
                        f (x:xs) = RecordExpression (M.fromList [(Symbol "head", x), (Symbol "tail", f xs)])

instance (Show d, ToLocalStat d) => ToJExpr (Expression d) where

    -- These are inline cheats to improve performance
    toJExpr (ApplyExpression (SymbolExpression (Operator "==")) [x, y]) = [jmacroE| _eq_eq(`(x)`)(`(y)`) |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "!=")) [x, y]) = [jmacroE| !_eq_eq(`(x)`)(`(y)`) |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "+")) [x, y])  = [jmacroE| `(x)` + `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "*")) [x, y])  = [jmacroE| `(x)` * `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "-")) [x, y])  = [jmacroE| `(x)` - `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "-")) [x])  = [jmacroE| 0 - `(x)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "&&")) [x, y]) = [jmacroE| `(x)` && `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "||")) [x, y]) = [jmacroE| `(x)` || `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "<=")) [x, y]) = [jmacroE| `(x)` <= `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator ">=")) [x, y]) = [jmacroE| `(x)` >= `(y)` |]

    toJExpr (ApplyExpression (SymbolExpression f @ (Operator _)) [x, y]) = 
        toJExpr (ApplyExpression (SymbolExpression (Symbol (to_name f))) [x,y])

    toJExpr (ApplyExpression (SymbolExpression (Operator _)) x) =
        error $ "Operator with " ++ show (length x) ++ " params"

    toJExpr (ApplyExpression (SymbolExpression (Symbol f)) []) = ref f
    toJExpr (ApplyExpression f []) = [jmacroE| `(f)` |]
    toJExpr (ApplyExpression f (end -> x : xs)) = [jmacroE| `(ApplyExpression f xs)`(`(x)`) |]

    toJExpr (ListExpression x)      = toJExpr x
    toJExpr (LiteralExpression l)   = toJExpr l
    toJExpr (SymbolExpression (Symbol x))    = ref x
    toJExpr (FunctionExpression x)  = toJExpr x
    toJExpr (LazyExpression x Every)      = toJExpr (FunctionExpression 
                                                     [ EqualityAxiom 
                                                       (Match [AnyPattern] Nothing)
                                                       x ])

    toJExpr (LazyExpression (Addr _ _ x) Once) =

        [jmacroE| (function() {
                      var y = undefined;
                      return function() {
                          if (y) {
                              return y;
                          } else {
                              y = `(x)`;
                              return y;
                          }
                      }
                  })() |]

    toJExpr (RecordExpression m)    = toJExpr (M.mapKeys show m)
    toJExpr (JSExpression s)        = s
    toJExpr (LetExpression bs ex)   = [jmacroE| (function() { `(foldl1 mappend $ map toLocal bs)`; return `(ex)` })() |]

    toJExpr (IfExpression x y z)    = [jmacroE| (function(){ 
                                                    if (`(x)`) { 
                                                        return `(y)`;
                                                    } else { 
                                                        return `(z)` 
                                                    }
                                                 })() |] 

    toJExpr x = error $ "Unimplemented " ++ show x

