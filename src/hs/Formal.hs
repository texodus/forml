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
import System.Exit
import System.Console.ANSI

import Network.HTTP
import Network.URI

import Text.Pandoc

import Data.Char (ord, isAscii)
import Data.String.Utils
import Data.URLEncoded

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

monitor :: String -> IO (Either [String] a) -> IO a
monitor x d = do putStr$ "[ ] " ++ x
                 hFlush stdout
                 d' <- d
                 case d' of
                   Right y -> do putStr "\r["
                                 setSGR [SetColor Foreground Dull Green]
                                 putStr "*"
                                 setSGR []
                                 putStrLn$ "] " ++ x
                                 return y
                   Left y  -> do putStr "\r["
                                 setSGR [SetColor Foreground Dull Red]
                                 putStr "X"
                                 setSGR []
                                 putStrLn$ "] " ++ x
                                 putStrLn ""
                                 mapM putStrLn y
                                 exitFailure

main :: IO ()
main  = do RunConfig (file:_) output _ <- parseArgs <$> getArgs
           hFile  <- openFile file ReadMode
           src <- (\ x -> x ++ "\n") <$> hGetContents hFile
           src' <- monitor "Parsing" . return$
                  case parseFormal src of
                    Left ex -> Left [show ex]
                    Right x -> Right x
           monitor "Type Checking" . return $
                   case tiProgram src' of
                     (_, []) -> Right ()
                     (_, y)  -> Left y
           (js, tests) <- monitor "Generating Javascript"$
                   do let tests = case src' of (Program xs) -> get_tests xs
                      let html = highlight tests $ toHTML (annotate_tests src src')
                      writeFile (output ++ ".html") html
                      let js = compress $ render src'
                      return$ Right (js, render_spec src')
           opt <- monitor "Optimizing"$ closure js
           writeFile (output ++ ".js") opt
           writeFile (output ++ ".spec.js") tests
           return ()

data RunMode   = Compile | JustTypeCheck
data RunConfig = RunConfig [String] String RunMode

closure :: String -> IO (Either [String] String)
closure x = do let uri = case parseURI "http://closure-compiler.appspot.com/compile" of Just x -> x

               let y = export$ importList [ ("output_format", "text")
                                          , ("output_info", "compiled_code")
                                          , ("compilation_level", "ADVANCED_OPTIMIZATIONS")
                                          , ("js_code", x) ]

               let args = [ mkHeader HdrContentLength (show$ length y)
                          , mkHeader HdrContentType "application/x-www-form-urlencoded" ]

               rsp <- simpleHTTP (Request uri POST args y)
               txt <- getResponseBody rsp
               return$ Right txt
                                         

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

-- wrap_html :: String -> String -> String
-- wrap_html name body = [qq|

--  <!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN' 'http://www.w3.org/TR/html4/loose.dtd'>
--  <html>
--  <head>
--  <title>$name</title>
--  <link rel='stylesheet' type='text/css' href='lib/js/jasmine-1.0.1/jasmine.css'>
--  <script type='text/javascript' src='lib/js/jasmine-1.0.1/jasmine.js'></script>
--  <script type='text/javascript' src='lib/js/jasmine-1.0.1/jasmine-html.js'></script>
--  <script type='text/javascript' src='lib/js/zepto.js'></script>
--  <script type='text/javascript' src='src/js/FormalReporter.js'></script>
--  <script type='text/javascript' src='src/js/table_of_contents.js'></script>
--  <script type='text/javascript' src='$name.js'></script>
--  <script type='text/javascript' src='$name.spec.js'></script>
--  <link href='http://kevinburke.bitbucket.org/markdowncss/markdown.css' rel='stylesheet'></link>
--  <link href='lib/js/prettify.css' type='text/css' rel='stylesheet' />
--  <script type='text/javascript' src='lib/js/prettify.js'></script>
--  <script type='text/javascript' src='lib/js/lang-hs.js'></script>
--  <script type='text/javascript' src='lib/js/jquery.js'></script>
--  <style>
--      ul\{
--          padding-left:40px;
--      \};
--      .jasmine_reporter\{
--          position: absolute;
--          bottom: 0px;
--      \}
--      a, a:visited, a:active, a:link \{
--          text-decoration: none;
--          color: #cccccc;
--      \}
--      .jasmine_reporter a, .jasmine_reporter a:visited, .jasmine_reporter a:active, .jasmine_reporter a:link \{
--          text-decoration: none;
--          color: #000000;
--      \}


--  </style>
--  </head>
--  <body>
--  <div style='position:fixed;top:0px;bottom:0px;width:100%;background:none;padding:105px 0px 0px 20px;line-height:1.5'>
--  <div style='position:absolute;left:0px;width:15%'>
--  <div id="contents" style='float:right'>
--  </div></div></div>
--  <div style='position:absolute;top:0px;left:0px;margin-left:15%; width:85%'>
--  <div id='main' style='margin: 0 0 50px 10%'>$body</div>
--  </div>
--  </body>
--  </html>

-- |]

