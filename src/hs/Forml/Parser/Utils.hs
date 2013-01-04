{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}



module Forml.Parser.Utils where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State hiding (lift)

import Text.Parsec hiding ((<|>), State, many, spaces, parse, label)
import Text.Parsec.Pos
import qualified Text.Parsec as P
import Text.Parsec.Indent  hiding (same)

import qualified Data.Map as M
import qualified Data.List as L

import Data.String.Utils
import qualified Data.Serialize as S

import GHC.Generics

import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)




data Addr a = Addr SourcePos SourcePos a deriving (Eq, Generic)

instance (S.Serialize a) => S.Serialize (Addr a)
instance S.Serialize SourcePos where

    get = return $ newPos "Serialized Optimization" 0 0
    put _ = return ()


instance Functor Addr where

    fmap f (Addr s e a) = Addr s e $ f a

addr :: Parser a -> Parser (Addr a)
addr p = do x <- getPosition
            y <- p
            z <- getPosition
            return$ Addr x z y


instance (Show a) => Show (Addr a) where
    show (Addr _ _ x) = show x

instance (Monad m) => Stream T.Text m Char where
    uncons = return . T.uncons

type Parser a = ParsecT T.Text () (StateT SourcePos Identity) a

class Syntax a where
    syntax :: Parser a

instance Syntax Double where
    syntax = read <$> do x <- many1 digit
                         string "."
                         y <- many1 digit
                         return $ x ++ "." ++ y

instance Syntax Int where
    syntax = read <$> many1 digit

instance Syntax String where
    syntax = do char '"'
                (escaped_char <|> anyChar) `manyTill` char '"'

        where escaped_char = do char '\\'
                                x <- oneOf "tnr\\"
                                case x of
                                  'r' -> return '\r'
                                  'n' -> return '\n'
                                  't' -> return '\t'
                                  '\\' -> return '\\'
                                  _   -> error "Unimplemented"


parse :: Parser a -> SourceName -> String -> Either ParseError a
parse parser sname input = runIndent sname $ runParserT parser () sname (T.pack input)

whitespace :: Parser String
whitespace = many $ oneOf "\t "

whitespace1 :: Parser String
whitespace1 = space >> whitespace

same :: Parser ()
same = spaces >> do pos <- getPosition
                    s <- get
                    if (sourceColumn pos) /= (sourceColumn s)
                       then parserFail $ "indented to exactly " ++ show (sourceColumn s + 1)
                       else do put $ setSourceLine s (sourceLine pos)
                               return ()

set_indentation :: (Int -> Int) -> Parser ()
set_indentation f = do s <- get
                       put$ setSourceColumn s (f (sourceColumn s))

spaces :: Parser ()
spaces = try emptyline `manyTill` try line_start >> return ()

    where emptyline  = whitespace >> newline
          line_start = whitespace >> notFollowedBy newline >> in_block

          in_block = do pos <- getPosition
                        s <- get
                        if (sourceColumn pos) < (sourceColumn s)
                           then parserFail $ "indented to at least " ++ show (sourceColumn s + 1)
                           else do put $ setSourceLine s (sourceLine pos)
                                   return ()


comment :: Parser String
comment = try empty_line <|> try commented_code <|> try code <|> markdown_comment

    where markdown_comment = anyChar `manyTill` newline *> return "\n"
          empty_line = whitespace *> newline *> return "\n"
          code = (\x y -> x ++ rstrip y ++ "\n")
              <$> string "    " <*> (anyChar `manyTill` newline)

          commented_code = do

              string "    "
              x <- noneOf "\n" `manyTill` try (string "--")
              anyChar `manyTill` newline
              return $ if length (strip x) > 0
                       then "    " ++ rstrip x ++ "\n"
                       else "\n"

sep_with :: Show a => String -> [a] -> String
sep_with x = concat . L.intersperse x . fmap show

unsep_with :: forall a b. (Show a, Show b) => String -> (M.Map b a) -> String
unsep_with z = concat . L.intersperse ", " . fmap (\(x, y) -> concat [show x, z, show y]) . M.toAscList

(<:>) :: Parser a -> Parser [a] -> Parser [a]
(<:>) x y = (:) <$> x <*> y

operator_dict :: M.Map Char String
operator_dict = M.fromList [ ('!', "_bang"),
                             ('@', "_at"),
                             ('#', "_hash"),
                             ('$', "$"),
                             ('%', "_perc"),
                             ('^', "_exp"),
                             ('&', "_and"),
                             ('|', "_or"),
                             ('<', "_less"),
                             ('>', "_grea"),
                             ('?', "_ques"),
                             ('/', "_forw"),
                             ('=', "_eq"),
                             (':', "_col"),
                             ('\\', "_back"),
                             ('~', "_tild"),
                             ('+', "_plus"),
                             ('-', "_minu"),
                             ('\'', "_tick"),
                             ('*', "_star"),
                             (',', "_comm"),
                             ('\'', "_apos"),
                             ('.', "_comp") ]

operator :: Parser Char
operator = oneOf (fst . unzip . M.toList $ operator_dict)

not_reserved :: Parser String -> Parser String
not_reserved x = do y <- x
                    if y `elem` reserved_words
                       then parserFail "non-reserved word"
                       else return y

    where reserved_words = [ "if", "then", "else", "let", "when", "with", "and", "or", "do", "do!", "var",
                             "module", "open", "yield", "lazy", "inline", "in", "is", "isnt",
                             "|", "\\", "=", ":", ",", "->", "<-" ]

not_system :: Parser String -> Parser String
not_system x = not_reserved$ do y <- x
                                if y `elem` reserved_words
                                    then parserFail "non-reserved word"
                                    else return y

    where reserved_words = [ "==", "<=", ">=", "!=", "<", ">", "||", "&&", ".", ","  ]

valid_partial_op :: Parser String -> Parser String
valid_partial_op x = not_reserved$

                             do y <- x
                                if y `elem` reserved_words
                                    then parserFail "non-reserved word"
                                    else return y

    where reserved_words = [ ".", ","  ]


type_sep     :: Parser Char
indentPairs  :: String -> Parser a -> String -> Parser a
not_comma    :: Parser ()
comma        :: Parser ()
optional_sep :: ParsecT T.Text () (StateT SourcePos Identity) ()

type_sep          = try (spaces *> char '|' <* whitespace)
not_comma         = whitespace >> newline >> spaces >> notFollowedBy (string "}")
comma             = P.spaces *> string "," *> P.spaces
optional_sep      = try (try comma <|> not_comma)

indentPairs a p b = string a *> P.spaces *> (try p <|> withPosTemp p) <* P.spaces <* string b

indentAsymmetricPairs :: String -> Parser a -> Parser b -> Parser a
indentAsymmetricPairs a p b = string a *> P.spaces *> withPosTemp p <* P.spaces <* b

withPosTemp :: Parser a -> Parser a
withPosTemp p = do x <- get
                   p' <- try (Just <$> withPos p) <|> return Nothing
                   put x
                   case p' of
                       Just p' -> return p'
                       Nothing -> parserFail ("expression continuation indented to " ++ show x)


db :: Show a => a -> a
db x = unsafePerformIO $ do putStrLn$ "-- " ++ (show x)
                            return x

