{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE ViewPatterns         #-}


module Forml.Doc where

import Text.InterpolatedString.Perl6
import Text.Pandoc

import Data.Char         (isAscii, ord)
import Data.String.Utils



toEntities :: String -> String
toEntities [] = ""
toEntities (c:cs) | isAscii c = c : toEntities cs
                  | otherwise = [qq|&#{ord c};{toEntities cs}|]

toHTML :: String -> String
toHTML = toEntities . writeHtmlString defaultWriterOptions . readMarkdown defaultParserState


to_literate :: String -> String -> String
to_literate "(Prelude)" = id
to_literate filename
    | (head . tail . split "." $ filename) == "forml" = unlines . map l . lines
    | otherwise = id

    where l (lstrip -> '-':'-':xs) = lstrip xs
          l x = "    " ++ x


get_title :: String -> (String, String)
get_title x = case lines x of
                  z @ ((strip -> ('-':'-':_:x')):('-':'-':_:'-':_):_) ->
                      (x', unlines $ drop 2 z)
                  _ -> ("<i>Untitled Program</i>", x)

