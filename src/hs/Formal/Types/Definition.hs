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

module Formal.Types.Definition where

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



-- Definition
-- --------------------------------------------------------------------------------

data Definition = Definition Symbol [Axiom]

instance Show Definition where
    show (Definition name ax) =[qq|$name {sep_with "\\n" ax}|]

instance Syntax Definition where

    syntax = do whitespace
                name <- try syntax <|> (Symbol <$> many1 (char '_'))
                sig <- first
                eqs <- (try $ spaces *> (withPos . many . try $ eq_axiom)) <|> return []
                whitespace
                if length sig == 0 && length eqs == 0 
                    then parserFail "Definition Axioms"
                    else return $ Definition name (sig ++ eqs)

        where first = try type_or_first
                      <|> ((:[]) <$> try naked_eq_axiom) 
                      <|> return []

              type_or_first = (:) <$> type_axiom <*> second

              second = option [] ((:[]) <$> try (no_args_eq_axiom (Match [] Nothing)))

              eq_axiom   = do try (spaces >> same) <|> (whitespace >> return ())
                              string "|"
                              naked_eq_axiom

              naked_eq_axiom = do whitespace
                                  patterns <- syntax
                                  no_args_eq_axiom patterns

              no_args_eq_axiom patterns = do whitespace *> string "=" *> spaces *> indented
                                             ex <- withPos syntax
                                             return $ EqualityAxiom patterns ex

              type_axiom = do spaces
                              indented
                              string ":"
                              spaces
                              indented
                              TypeAxiom <$> withPos type_axiom_signature

instance ToStat Definition where
    toStat (Definition name as) = declare_this (to_name name) $ toJExpr as

newtype Local = Local Definition

instance ToStat Local where
    toStat (Local (Definition name as)) = declare (to_name name) $ toJExpr as



-- Axiom
-- --------------------------------------------------------------------------------

data Axiom = TypeAxiom UnionType
           | EqualityAxiom Match Expression

instance Show Axiom where
    show (TypeAxiom x) = ": " ++ show x
    show (EqualityAxiom ps ex) = [qq|$ps = $ex|]

instance ToJExpr [Axiom] where
    toJExpr [] = toJExpr . scope . Curried $ []
    toJExpr (TypeAxiom _:xs) = toJExpr xs
    toJExpr xs @ (EqualityAxiom (Match ps _) _ : _) = scope . curry (length ps) . toStat . Curried $ xs

newtype Curried = Curried [Axiom]

instance ToStat Curried where
    toStat (Curried []) = [jmacro| args = []; exhaust(); |]
    toStat (Curried (EqualityAxiom (Match pss cond) ex : xss)) = 

        [jmacro| `(declare_bindings pss)`;
                 if (`(pss)` && `(cond)`) {
                     return `(ex)`;
                 } else `(Curried xss)`; |]


            where declare_bindings [] = mempty
                  declare_bindings (VarPattern x : zs) = declare x [jmacroE| null |] ++ declare_bindings zs
                  declare_bindings (RecordPattern x : zs) = 
                      let (_, z) = unzip . M.toList $ x
                      in  declare_bindings z ++ declare_bindings zs

                  declare_bindings (_ : zs) = declare_bindings zs


-- Pattern
-- --------------------------------------------------------------------------------

data Match = Match [Pattern] (Maybe Expression)

instance Show Match where
    show (Match p Nothing)  = sep_with " " p
    show (Match p (Just x)) = [qq|{sep_with " " p} when $x|]

instance Syntax Match where

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

data PatternMatch = PM String Pattern

instance ToJExpr [PatternMatch] where
    toJExpr []     = toJExpr True
    toJExpr (x:[]) = [jmacroE| `(x)` |]
    toJExpr (x:xs) = [jmacroE| `(x)` && `(xs)` |]

instance ToJExpr PatternMatch where
    toJExpr (PM _ AnyPattern) = toJExpr True
    toJExpr (PM n (VarPattern x)) = 
        [jmacroE| (function() {
                     `(ref x)` = `(ref n)`; 
                     return true; 
                   })() |]

    toJExpr (PM n (LiteralPattern x))               = [jmacroE| `(ref n)` === `(x)` |]
    toJExpr (PM _ (RecordPattern (M.toList -> []))) = [jmacroE| true |]
    toJExpr (PM n (RecordPattern (M.toList -> xs))) = [jmacroE| `(map g xs)` && `(map f xs)` |]
            where f (key, val) = PM (n ++ "[\"" ++ to_name key ++ "\"]") val
                  g (key, _) = Condition [jmacroE| `(ref n)`.hasOwnProperty(`(to_name key)`) |]

    toJExpr (PM n (ListPattern []))                 = [jmacroE| equals(`(n)`)([]) |]
    toJExpr (PM n (ListPattern xs)) = 
        let x = toJExpr (map (\(index, val) -> toJExpr (PM (n ++ "[" ++ show index ++ "]") val)) (zip [0..] xs))
        in   [jmacroE| `(x)` && `(ref n)`.length == `(length xs)` |]
    toJExpr (PM _ x) = error $ "Unimplemented " ++ show x


data Pattern = VarPattern String
             | AnyPattern
             | LiteralPattern Literal
             | RecordPattern (M.Map Symbol Pattern)
             | ListPattern [Pattern]
             | ViewPattern Expression Pattern

instance Show Pattern where
    show (VarPattern x)     = x
    show AnyPattern         = "_"
    show (LiteralPattern x) = show x
    show (ListPattern x)    = [qq|[ {sep_with ", " x} ]|]
    show (ViewPattern x y)  = [qq|($x -> $y)|]
    show (RecordPattern m)  = [qq|\{ {unsep_with " = " m} \}|] 

instance ToJExpr [Pattern] where
    toJExpr ps = toJExpr $ zipWith PM (reverse . take (length ps) . map local_pool $ [0 .. 26]) ps

instance Syntax Pattern where
    
    syntax = try literal_pattern
             <|> try naked_apply_pattern
             <|> try var_pattern
             <|> any_pattern
             <|> record_pattern
             <|> array_pattern
             <|> list_pattern
             <|> indentPairs "(" (try view_pattern <|> try apply_pattern <|> syntax) ")"

        where view_pattern        = ViewPattern <$> syntax <* spaces <* string "->" <* whitespace <*> syntax 
              var_pattern         = VarPattern <$> type_var
              literal_pattern     = LiteralPattern <$> syntax          
              any_pattern         = many1 (string "_") *> return AnyPattern
              naked_apply_pattern = 

                  do x <- many1 letter
                     string ":"
                     return $ RecordPattern (M.fromList [(Symbol x, AnyPattern )])

              apply_pattern = do x <- many1 letter 
                                 string ":" 
                                 whitespace
                                 y <- syntax
                                 return $ RecordPattern (M.fromList [(Symbol x, y)])

              record_pattern  = RecordPattern . M.fromList <$> indentPairs "{" pairs' "}"

                  where pairs' = key_eq_val `sepEndBy` try (comma <|> not_comma)
                        key_eq_val = do key <- syntax
                                        spaces
                                        string "=" <|> string ":"
                                        spaces
                                        value <- syntax
                                        return (key, value)

              list_pattern = ListPattern <$> indentPairs "[" (syntax `sepBy` try (try comma <|> not_comma)) "]"

              array_pattern = f <$> indentAsymmetricPairs "[:" v (try (string ":]") <|> string "]")

                  where v = do whitespace
                               withPos (syntax `sepBy` try (try comma <|> not_comma))

                        f [] = RecordPattern (M.fromList [(Symbol "nil", AnyPattern)])
                        f (x:xs) = RecordPattern (M.fromList [(Symbol "head", x), (Symbol "tail", f xs)])

instance ToJExpr (Maybe Expression) where
    toJExpr = maybe (toJExpr True) toJExpr




-- Expression
-- --------------------------------------------------------------------------------

data Expression = ApplyExpression Expression [Expression]
                | IfExpression Expression Expression Expression
                | LiteralExpression Literal
                | SymbolExpression Symbol
                | JSExpression JExpr
                | FunctionExpression [Axiom]
                | RecordExpression (M.Map Symbol Expression)
                | InheritExpression Expression (M.Map Symbol Expression)
                | LetExpression [Definition] Expression
                | ListExpression [Expression]

instance Show Expression where

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
    show (LetExpression ax e)    = replace "\n |" "\n     |" $ [qq|let {sep_with "\\n| " ax} in ($e)|]
    show (RecordExpression m)    = [qq|\{ {unsep_with " = " m} \}|] 
    show (InheritExpression x m) = [qq|\{ $x with {unsep_with " = " m} \}|] 


instance Syntax Expression where

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

              do'  = do string "do"
                        whitespace1
                        withPos line

                  where line = try bind <|> try let_bind <|> try return'

                        bind = do p <- syntax
                                  whitespace <* (string "<-" <|> string "←") <* whitespace 
                                  ex <- withPos syntax 
                                  spaces *> same
                                  f ex p <$> line

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
                                         f v AnyPattern <$> line

                        f ex pat zx = ApplyExpression 
                                         (SymbolExpression (Operator ">>="))
                                         [ ex, (FunctionExpression 
                                                    [ EqualityAxiom 
                                                      (Match [pat] Nothing)
                                                      zx ]) ]

              lazy  = do string "lazy"
                         whitespace1
                         f <$> withPos (try syntax)

                  where f ex = (FunctionExpression 
                                   [ EqualityAxiom 
                                     (Match [AnyPattern] Nothing)
                                     ex ])

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
                                 , [ix "+", ix "-"]
                                 , [ Infix user_op_right AssocRight, Infix user_op_left AssocLeft ]
                                 , [ix "<", ix "<=", ix ">=", ix ">", ix "==", ix "!="]
                                 , [ix "&&", ix "||", ix "and", ix "or" ] ]

                        ix s   = Infix (try . op $ (Operator <$> string s) <* notFollowedBy operator) AssocLeft
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

              accessor = do x <- indentPairs "(" syntax ")" 
                                 <|> js 
                                 <|> record 
                                 <|> literal
                                 <|> symbol
                                 <|> list

                            string "."
                            z <- syntax
                            return $ acc_exp x z

              acc_exp x z = ApplyExpression 
                            (FunctionExpression 
                             [ EqualityAxiom 
                               (Match [RecordPattern (M.fromList [(z, VarPattern "x")])] Nothing)
                               (SymbolExpression (Symbol "x")) ] )
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

              function = withPosTemp $ do try (char '\\') <|> char '§' <|> char 'λ'
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
                                        ex <- withPos syntax
                                        return $ EqualityAxiom patterns ex

              js = JSExpression <$> join (p <$> indentPairs "`" (many $ noneOf "`") "`")
                  where p (parseJM . wrap -> Right (BlockStat [AssignStat _ x])) =
                            return [jmacroE| (function() { return `(x)`; }) |]
                        p y @ (parseJM . wrap -> Left _)  =
                            case parseJM y of
                              Left _  -> parserFail "Javascript"
                              Right z -> return [jmacroE| (function() { `(z)`; }) |] 
                        
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
                                        ex <- withPos syntax
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

              literal = LiteralExpression <$> syntax
              symbol  = SymbolExpression <$> syntax

              list    = ListExpression <$> indentPairs "[" v "]"
                  where v = do whitespace
                               withPos (syntax `sepBy` try (try comma <|> not_comma))

              array   = f <$> indentAsymmetricPairs "[:" v (try (string ":]") <|> string "]")

                  where v = do whitespace
                               withPos (syntax `sepBy` try (try comma <|> not_comma))

                        f [] = RecordExpression (M.fromList [(Symbol "nil", SymbolExpression (Symbol "true"))])
                        f (x:xs) = RecordExpression (M.fromList [(Symbol "head", x), (Symbol "tail", f xs)])

instance ToJExpr Expression where

    -- These are inline cheats to improve performance
    toJExpr (ApplyExpression (SymbolExpression (Operator "==")) [x, y]) = [jmacroE| _eq_eq(`(x)`)(`(y)`) |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "!=")) [x, y]) = [jmacroE| !_eq_eq(`(x)`)(`(y)`) |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "+")) [x, y])  = [jmacroE| `(x)` + `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "*")) [x, y])  = [jmacroE| `(x)` * `(y)` |]
    toJExpr (ApplyExpression (SymbolExpression (Operator "-")) [x, y])  = [jmacroE| `(x)` - `(y)` |]
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
    toJExpr (RecordExpression m)    = toJExpr (M.mapKeys show m)
    toJExpr (JSExpression s)        = s
    toJExpr (LetExpression bs ex)   = [jmacroE| (function() { `(map Local bs)`; return `(ex)` })() |]

    toJExpr (IfExpression x y z)    = [jmacroE| (function(){ 
                                                    if (`(x)`) { 
                                                        return `(y)`;
                                                    } else { 
                                                        return `(z)` 
                                                    }
                                                 })() |] 

    toJExpr x = error $ "Unimplemented " ++ show x

