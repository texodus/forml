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


get_title :: String -> String -> (String, String)
get_title d x = case lines x of
                  z @ ((strip -> ('-':'-':_:x')):('-':'-':_:'-':_):_) ->
                      (x', unlines $ drop 2 z)
                  z @ ((strip -> ('-':'-':_:x')):('-':'-':_:'=':_):_) ->
                      (x', unlines $ drop 2 z)
                  _ -> (d, x)


docs :: String -> [String] -> [Title] -> [String] -> [Program] -> [Source] -> IO ()
docs _ [] [] [] [] [] = return ()
docs js (tests:testses) (filename':filenames) (title:titles) (program @ (Program xs):programs) (source:sources) =

  let html     = highlight (get_tests xs) $ toHTML (annotate_tests source program)

      filename = [qq|$filename'.html|]
      compiled = [qq|<script>$js $tests</script>|]
      hook     = [qq|<script>$htmljs;window.document.title='{title}';$('header h1').html('{title}')</script>|]

  in  do monitor [qq|Docs {filename}|] $
            do writeFile filename $ concat [header, css', scripts, compiled, html, hook, footer]
               return $ Right ()
         
         docs js testses filenames titles programs sources

docs _ _ _ _ _ _ = error "Paradox: `docs` called with non equivalent arguments"
