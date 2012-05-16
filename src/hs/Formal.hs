{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Main where

import Text.InterpolatedString.Perl6

import Control.Applicative
import Control.Monad.State hiding (lift)

import System.IO
import System.Environment

import Text.Pandoc

import Data.Char (ord, isAscii)
import Data.String.Utils

import Formal.Parser
import Formal.Javascript
import Formal.TypeCheck hiding (split)

-- Main
-- ----

toEntities :: String -> String
toEntities [] = ""
toEntities (c:cs) | isAscii c = c : toEntities cs
                  | otherwise = [qq|&#{ord c};{toEntities cs}|]

toHTML :: String -> String
toHTML = toEntities . writeHtmlString defaultWriterOptions . readMarkdown defaultParserState

main :: IO ()
main  = do RunConfig (file:_) output mode <- parseArgs <$> getArgs
           hFile  <- openFile file ReadMode
           src <- (\ x -> x ++ "\n") <$> hGetContents hFile
           putStr "[ ] Parsing"
           case parseFormal src of
             Left  ex   -> putStrLn "\r[X] Parsing" >> putStrLn (show ex)
             Right src' -> do putStrLn "\r[*] Parsing"
                              putStrLn $ show $ A $ tiProgram [] src'
                              let tests = case src' of (Program xs) -> get_tests xs
                              let html = highlight tests $ toHTML (wrap_html output (annotate_tests src src'))
                              writeFile (output ++ ".html") html
                              writeFile (output ++ ".raw.html") $ highlight tests $ toHTML (annotate_tests src src')
                              putStr "[ ] Generating Javascript"
                              let js = compress $ render src'
                              writeFile (output ++ ".js") js
                              putStrLn "\r[*] Generating Javascript"
                              putStr "[ ] Generating Tests"
                              let spec = render_spec src'
                              writeFile (output ++ ".spec.js") spec
                              putStrLn "\r[*] Generating Tests"

data RunMode   = Compile | JustTypeCheck
data RunConfig = RunConfig [String] String RunMode

parseArgs :: [String] -> RunConfig
parseArgs = fst . runState argsParser

  where argsParser = do args <- get
                        case args of
                          []     -> return $ RunConfig [] "default" Compile
                          (x:xs) -> do put xs
                                       case x of
                                         "-t"    -> do RunConfig a b _ <- argsParser
                                                       return $ RunConfig (x:a) b JustTypeCheck
                                         "-o"    -> do (name:ys) <- get
                                                       put ys
                                                       RunConfig a _ c <- argsParser
                                                       return $ RunConfig (x:a) name c
                                         ('-':_) -> do error "Could not parse options"
                                         z       -> do RunConfig a _ c <- argsParser
                                                       let b = last $ split "/" $ head $ split "." z
                                                       return $ RunConfig (x:a) b c


-- Docs

wrap_html :: String -> String -> String
wrap_html name body = [qq|

 <!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN' 'http://www.w3.org/TR/html4/loose.dtd'>
 <html>
 <head>
 <title>$name</title>
 <link rel='stylesheet' type='text/css' href='lib/js/jasmine-1.0.1/jasmine.css'>
 <script type='text/javascript' src='lib/js/jasmine-1.0.1/jasmine.js'></script>
 <script type='text/javascript' src='lib/js/jasmine-1.0.1/jasmine-html.js'></script>
 <script type='text/javascript' src='lib/js/zepto.js'></script>
 <script type='text/javascript' src='src/js/FormalReporter.js'></script>
 <script type='text/javascript' src='src/js/table_of_contents.js'></script>
 <script type='text/javascript' src='$name.js'></script>
 <script type='text/javascript' src='$name.spec.js'></script>
 <link href='http://kevinburke.bitbucket.org/markdowncss/markdown.css' rel='stylesheet'></link>
 <link href='lib/js/prettify.css' type='text/css' rel='stylesheet' />
 <script type='text/javascript' src='lib/js/prettify.js'></script>
 <script type='text/javascript' src='lib/js/lang-hs.js'></script>
 <script type='text/javascript' src='lib/js/jquery.js'></script>
 <style>
     ul\{
         padding-left:40px;
     \};
     .jasmine_reporter\{
         position: absolute;
         bottom: 0px;
     \}
     a, a:visited, a:active, a:link \{
         text-decoration: none;
         color: #cccccc;
     \}
     .jasmine_reporter a, .jasmine_reporter a:visited, .jasmine_reporter a:active, .jasmine_reporter a:link \{
         text-decoration: none;
         color: #000000;
     \}


 </style>
 </head>
 <body>
 <div style='position:fixed;top:0px;bottom:0px;width:100%;background:none;padding:105px 0px 0px 20px;line-height:1.5'>
 <div style='position:absolute;left:0px;width:15%'>
 <div id="contents" style='float:right'>
 </div></div></div>
 <div style='position:absolute;top:0px;left:0px;margin-left:15%; width:85%'>
 <div id='main' style='margin: 0 0 50px 10%'>$body</div>
 </div>
 </body>
 </html>

|]

