{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}


module Main where

import Text.InterpolatedString.Perl6

import Control.Applicative
import Control.Monad.State hiding (lift)

import System.IO
import System.Environment
import System.Exit
import System.Console.ANSI
import System.Directory

import Network.HTTP
import Network.URI
import Control.Concurrent

import Data.Monoid

import Text.Pandoc

import Data.Char (ord, isAscii)
import Data.List as L
import Data.String.Utils
import Data.URLEncoded

import Formal.Parser
import Formal.Javascript
import Formal.TypeCheck hiding (split)
--import Formal.Optimize
import Formal.Types.Namespace

import qualified Data.ByteString.Char8 as B

import Data.FileEmbed
import System.Process

-- Main
-- ----

toEntities :: String -> String
toEntities [] = ""
toEntities (c:cs) | isAscii c = c : toEntities cs
                  | otherwise = [qq|&#{ord c};{toEntities cs}|]

toHTML :: String -> String
toHTML = toEntities . writeHtmlString defaultWriterOptions . readMarkdown defaultParserState

warn :: String -> a -> IO a
warn x js = do putStr "\r["
               setSGR [SetColor Foreground Dull Yellow]
               putStr "-"
               setSGR []
               putStrLn$ "] " ++ x
               return js

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


to_literate :: String -> String -> String
to_literate filename
    | (head . tail . split "." $ filename) == "formal" = unlines . map l . lines
    | otherwise = id

    where l (lstrip -> '-':'-':xs) = lstrip xs
          l x = "    " ++ x


type TypeSystem = [(Namespace, [Assumption])]

to_parsed :: String -> TypeSystem -> Either [String] (TypeSystem, (Program, String))
to_parsed src env = case parseFormal src of
                      Left x  -> Left [show x]
                      Right x -> case tiProgram x env of
                                   (as, []) -> Right (as, (x, src))
                                   (_, y)   -> Left y

parse_formal :: [String] -> IO (TypeSystem, [(Program, String)])
parse_formal xs = foldM parse' ([], []) xs

    where parse' :: (TypeSystem, [(Program, String)]) -> String -> IO (TypeSystem, [(Program, String)])
          parse' (ts, as) filename = do hFile <- openFile filename ReadMode
                                        src <-   hGetContents hFile
                                        let src' = to_literate filename . (++ "\n") $ src
                                        (ts', as') <- monitor [qq|Loading $filename|] $ return$ to_parsed src' ts
                                        return (ts ++ ts', as ++ [as'])

gen_js :: [(Program, String)] -> (String, [(String, String)])
gen_js ps = let p = Program$ get_program ps
            in  (compress (read (render p)), [("", read (render_spec p))])

    where get_program ((Program ss, _): ps) = ss ++ get_program ps
          get_program [] = []

main :: IO ()
main  = do rc <- parseArgs <$> getArgs
           if watch rc then watch' rc
                       else compile rc

    where f (x, y) = show x ++ "\n    " ++ concat (L.intersperse "\n    " (map show y)) ++ "\n\n  "

          jquery  = $(embedFile "lib/js/jquery.js")
          header  = $(embedFile "src/html/header.html")
          footer  = $(embedFile "src/html/footer.html")
          jasmine = $(embedFile "lib/js/jasmine-1.0.1/jasmine.js")
                      `mappend` $(embedFile "lib/js/jasmine-1.0.1/jasmine-html.js")

          console = "prelude.html.console()"
          report  = $(embedFile "src/js/FormalReporter.js")
          htmljs  = "prelude.html.table_of_contents()"

          watch' rc =

              do x <- mapM getModificationTime . inputs $ rc
                 compile rc
                 putStr "Waiting ..."
                 hFlush stdout
                 wait rc x

          wait rc x =

              do threadDelay 1000
                 x' <- mapM getModificationTime . inputs $ rc
                 if x /= x' then do putStr "\r"
                                    watch' rc
                            else wait rc x

          compile rc =

              do (as, src') <- parse_formal$ inputs rc
                 let (js, ((_, tests):_)) = gen_js src'
   
                 js <- if optimize rc 
                       then (monitor "Optimizing"$ closure js)
                       else do warn "Optimizing" js
   
                 writeFile (output rc ++ ".js") js
                 writeFile (output rc ++ ".spec.js") tests

                 let xxx = fst . head $ src'
                 let yyy = snd . head $ src'
                 let html = highlight (case xxx of (Program xs) -> get_tests xs)$ toHTML (annotate_tests yyy xxx)
                 let prelude = "<script>" ++ B.unpack jasmine ++ B.unpack report ++ js ++ tests ++ "</script>"
                 let hook = "<script>" ++ htmljs ++ "</script>"
                 writeFile ((output rc) ++ ".html") (B.unpack header ++ prelude ++ html ++ hook ++ B.unpack footer)

                 case run_tests rc of
                     Node ->
                          monitor "Testing"$
                          do (Just std_in, Just std_out, _, p) <-
                                 createProcess (proc "node" []) { std_in = CreatePipe, std_out = CreatePipe }
                             hPutStrLn std_in$ B.unpack jasmine
                             hPutStrLn std_in$ js ++ "\n\n"
                             hPutStrLn std_in$ tests
                             hPutStrLn std_in$ console
                             z <- waitForProcess p

                             case z of 
                               ExitFailure _ -> return$ Left []
                               ExitSuccess -> if (show_types rc) 
                                              then Right <$> putStrLn ("\nTypes\n\n  " ++ concat (map f as))
                                              else return$ Right ()

                     Phantom -> 
                          monitor "Testing"$
                          do writeFile (output rc ++ ".phantom.js")
                                   (B.unpack jquery ++ B.unpack jasmine ++ js ++ tests ++ console)
                            

                             (z, msg, _) <- readProcessWithExitCode "phantomjs" [output rc ++ ".phantom.js"] ""
                             system$ "rm " ++ output rc ++ ".phantom.js"

                             case z of 
                               ExitFailure _ -> return$ Left [msg]
                               ExitSuccess -> if (show_types rc) 
                                              then Right <$> putStrLn ("\nTypes\n\n  " ++ concat (map f as))
                                              else return$ Right ()
                     NoTest ->
                          do warn "Testing" ()
                             if (show_types rc) 
                               then putStrLn$ "\nTypes\n\n  " ++ concat (map f as)
                               else return ()


closure :: String -> IO (Either a String)
closure x = do let uri = case parseURI "http://closure-compiler.appspot.com/compile" of Just x -> x

                   y = export$ importList [ ("output_format",     "text")
                                          , ("output_info",       "compiled_code")
                                          , ("compilation_level", "ADVANCED_OPTIMIZATIONS")
                                          , ("js_code",           x) ]

                   args = [ mkHeader HdrContentLength (show$ length y)
                          , mkHeader HdrContentType "application/x-www-form-urlencoded" ]

               rsp <- simpleHTTP (Request uri POST args y)
               txt <- getResponseBody rsp

               return$ Right txt

data TestMode = NoTest | Node | Phantom
                                         
data RunConfig = RunConfig { inputs :: [String]
                           , output :: String
                           , show_types :: Bool
                           , optimize :: Bool
                           , run_tests :: TestMode 
                           , write_docs :: Bool
                           , watch :: Bool }

parseArgs :: [String] -> RunConfig
parseArgs = fst . runState argsParser

  where argsParser = do args <- get
                        case args of
                          []     -> return $ RunConfig [] "default" False True Phantom True False
                          (x:xs) -> do put xs
                                       case x of
                                         "-w"    -> do x <- argsParser
                                                       return $ x { watch = True }
                                         "-t"    -> do x <- argsParser
                                                       return $ x { show_types = True }
                                         "-no-opt" -> do x <- argsParser
                                                         return $ x { optimize = False }
                                         "-no-test" -> do x <- argsParser
                                                          return $ x { run_tests = NoTest }
                                         "-node-test" -> do x <- argsParser
                                                            return $ x { run_tests = Node }
                                         "-o"    -> do (name:ys) <- get
                                                       put ys
                                                       RunConfig a _ c d e f g <- argsParser
                                                       return $ RunConfig (x:a) name c d e f g
                                         ('-':_) -> error "Could not parse options"
                                         z       -> do RunConfig a _ c d e f g <- argsParser
                                                       let b = last $ split "/" $ head $ split "." z
                                                       
                                                       return $ RunConfig (x:a) b c d e f g

