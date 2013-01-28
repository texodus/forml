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
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Readers.Markdown

import Data.Char         (isAscii, ord)
import Data.String.Utils


import Text.StringTemplate

import Forml.Parser
import Forml.Static
import Forml.CLI

type Source   = String
type Title    = String
type Error    = String
type Filename = String

toEntities :: String -> String
toEntities [] = ""
toEntities (c:cs) | isAscii c = c : toEntities cs
                  | otherwise = [qq|&#{ord c};{toEntities cs}|]

toHTML :: String -> String
toHTML = toEntities . writeHtmlString def . readMarkdown def


to_literate :: String -> String -> String
to_literate "(Prelude)" = id
to_literate filename
    | (head . reverse . split "." $ filename) == "lformal" = id
    | otherwise = unlines . map l . lines

    where l (lstrip -> '-':'-':xs) = lstrip xs
          l x = "    " ++ x


get_title :: String -> String -> (String, String, String)
get_title d x = case lines x of
    z @ ((strip -> ('-':'-':_:x')):('-':'-':_:'-':_):_) ->
        let description = get_description (drop 2 z) in
        (x', unlines description, unlines $ drop (2 + length description) z)
    z @ ((strip -> ('-':'-':_:x')):('-':'-':_:'=':_):_) ->
        let description = get_description (drop 2 z) in
        (x', unlines description, unlines $ drop (2 + length description) z)
    _ -> (d, "", x)

    where
        get_description (('-':'-':_:x):xs) = x : get_description xs
        get_description _ = []


docs :: String -> [String] -> [String] -> [Title] -> [String] -> [Program] -> [Source] -> IO ()
docs _ [] [] [] [] [] [] = return ()
docs js (tests:testses) (filename':filenames) (title:titles) (desc:descs) (program @ (Program xs):programs) (source:sources) =

  let html'     = highlight (get_tests xs) $ toHTML (annotate_tests source program)

      filename = [qq|$filename'.html|]
      compiled = [qq|<script>$js $tests</script>|]

      template = newSTMP html_template :: StringTemplate String
      html = render $
          setAttribute "html" (html' :: String) $
          setAttribute "scripts" (scripts) $
          setAttribute "javascript" (compiled :: String) $
          setAttribute "css" (css' :: String) $
          setAttribute "title" title $
          setAttribute "desc" (toHTML desc) $
          template

  in  do monitor [qq|Docs {filename}|] $
            do writeFile filename html
               return $ Right ()
         
         docs js testses filenames titles descs programs sources

docs _ _ _ _ _ _ _ = error "Paradox: `docs` called with non equivalent arguments"
