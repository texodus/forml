{-# LANGUAGE FlexibleContexts, NamedFieldPuns, OverlappingInstances,
             QuasiQuotes, RankNTypes, RecordWildCards, TemplateHaskell,
             TupleSections, ViewPatterns #-}


module Main(main) where

import Text.InterpolatedString.Perl6

import Control.Applicative
import Control.Monad.State hiding (lift)

import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

import Control.Concurrent

import Text.Pandoc

import Data.Char         (isAscii, ord)
import Data.List         as L
import Data.Monoid
import Data.String.Utils

import Forml.Closure
import Forml.Javascript
import Forml.Javascript.Backend
import Forml.Javascript.Utils   (prelude)
import Forml.Parser
import Forml.TypeCheck          hiding (split)
import Forml.Types.Statement
import Forml.CLI

import qualified Forml.Optimize as O

--import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as B
import           Data.FileEmbed



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

to_parsed :: String -> String -> TypeSystem -> Either [String] (TypeSystem, (Program, String))
to_parsed name src env = case parseForml name src of
                              Left x  -> Left [show x]
                              Right x -> case tiProgram x env of
                                           (as, []) -> Right (as, (x, src))
                                           (_, y)   -> Left y

prelude' = B.toString $(embedFile "src/forml/prelude.forml")


parse_forml :: [String] -> IO ([String], TypeSystem, [(Program, String)], [String])
parse_forml xs = do xs' <- mapM get_source xs 
                    foldM parse' ("prelude.forml" : xs, [], [], []) (prelude' : xs')

    where parse' :: ([String], TypeSystem, [(Program, String)], [String]) -> String -> IO ([String], TypeSystem, [(Program, String)], [String])
          parse' (zs, ts, as, titles) src'' =
          
             do let (title, src) = get_title src''
                let src' = to_literate (head zs) . (++ "\n") $ src
                (ts', as') <- monitor [qq|Loading {head zs}|] $ return$ to_parsed (head zs) src' ts
                return (tail zs, ts ++ ts', as ++ [as'], titles ++ [title])

                
          get_source filename =
             do hFile <- openFile filename ReadMode
                hGetContents hFile

gen_js :: [String] -> [Program] -> (String, [(String, String)])
gen_js src p = (compress (read' prelude ++ "\n" ++ (unlines $ map read' $ zipWith (render (Program $ get_program p)) src p)), [("",  read' prelude ++ "\n" ++ (unlines $ map read' $ zipWith (render_spec (Program $ get_program p)) src p))])

get_title :: String -> (String, String)
get_title x = case lines x of
                  z @ ((strip -> ('-':'-':_:x')):('-':'-':_:'-':_):_) -> 
                      (x', unlines $ drop 2 z)
                  _ -> ("<i>Untitled Program</i>", x)


read' :: [Char] -> [Char]
read' xs @ ('"':_) = read xs
read' x = x

get_program :: [Program] -> [Statement]
get_program (Program ss: ps) = ss ++ get_program ps
get_program [] = []

main :: IO ()
main  = do args <- getArgs
           main' args

main' :: [String] -> IO ()
main' (parseArgs -> rc') =
         if watch rc'
                 then watch' rc'
                 else compile rc'

    where f (x, y) = show x ++ "\n    " ++ concat (L.intersperse "\n    " (map show y)) ++ "\n\n  "

          jquery   = $(embedFile "lib/js/jquery.js")
          header   = $(embedFile "src/html/header.html")
          footer   = $(embedFile "src/html/footer.html")
          jasmine  = $(embedFile "lib/js/jasmine-1.0.1/jasmine.js") `mappend` $(embedFile "lib/js/jasmine-1.0.1/jasmine-html.js")
          prettify = $(embedFile "lib/js/prettify.js") `mappend` $(embedFile "lib/js/lang-hs.js")
          
          css      = $(embedFile "lib/js/jasmine-1.0.1/jasmine.css") `mappend` $(embedFile "src/html/styles.css") `mappend` $(embedFile "lib/js/prettify.css")
          report   = $(embedFile "src/js/FormlReporter.js")

          htmljs   = "$('code').addClass('prettyprint lang-hs');prettyPrint();$('#run_tests').bind('click', prelude.html.table_of_contents)"
          console  = "prelude.html.console_runner()"
          
--          assump   = case S.decode $(embedFile "prelude.types") of
--                       Left _ -> error "I Shouldn't be!"
--                       Right x -> x
          
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

              do (_, as, src', titles) <- parse_forml$ inputs rc
                 let src'' = O.run_optimizer (fmap fst src') as
                 let (js', ((_, tests'):_)) = gen_js (fmap snd src') src''
                 
               --  writeFile "prelude.types" (B.toString $ S.encode as)

                 js <- if optimize rc
                       then (monitor "Closure [libs"$ closure_local (js') "ADVANCED_OPTIMIZATIONS")
                       else do warn "Closure [libs]" js'

                 tests <- case rc of
                              RunConfig { optimize = True, run_tests = Phantom } ->
                                  monitor "Closure [tests"$ closure_local (tests') "SIMPLE_OPTIMIZATIONS"
                              _ -> warn "Closure [tests]" tests'

                 writeFile (output rc ++ ".js") js
                 writeFile (output rc ++ ".spec.js") tests

                 monitor "Docs" $
                     if write_docs rc
                     then let xxx = map fst src'
                              yyy = map snd src'
                              html' xxx' yyy' = highlight (case xxx' of (Program xs) -> get_tests xs)$ toHTML (annotate_tests yyy' xxx')
                              html = concat $ zipWith html' xxx yyy
                              prelude' = "<script>" ++ B.toString jquery ++ B.toString jasmine ++ B.toString report ++ B.toString prettify ++ js ++ tests ++ "</script>"
                              css' = "<style type=\"text/css\">" ++ B.toString css ++ "</style>"
                              hook = "<script>" ++ htmljs ++ ";window.document.title='"++(titles !! 0)++"';$('h1').html('" ++ (titles !! 0) ++ "')</script>"

                          in do writeFile ((output rc) ++ ".html") (B.toString header ++ css' ++ prelude' ++ html ++ hook ++ B.toString footer)
                                return $ Right ()
                     else return $ Right ()

                 case run_tests rc of
                     Node ->
                          monitor "Testing [Node.js]"$
                          do (Just std_in, Just std_out, _, p) <-
                                 createProcess (proc "node" []) { std_in = CreatePipe, std_out = CreatePipe }
                             forkIO $ do errors <- hGetContents std_out
                                         putStr errors
                                         hFlush stdout
                             hPutStrLn std_in$ B.toString jasmine
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
                          monitor "Testing [Phantom.js]"$
                          do writeFile (output rc ++ ".phantom.js")
                                   (B.toString jquery ++ B.toString jasmine ++ js ++ tests ++ console)

                             (Just std_in, Just std_out, _, p) <-
                                 createProcess (proc "phantomjs" [output rc ++ ".phantom.js"]) { std_in = CreatePipe, std_out = CreatePipe }
                             forkIO $ do errors <- hGetContents std_out
                                         putStr errors
                                         hFlush stdout
                             z <- waitForProcess p
                             system$ "rm " ++ output rc ++ ".phantom.js"

                             case z of
                               ExitFailure _ -> return$ Left []
                               ExitSuccess -> if (show_types rc)
                                              then Right <$> putStrLn ("\nTypes\n\n  " ++ concat (map f as))
                                              else return$ Right ()
                     NoTest ->
                          do warn "Testing" ()
                             if (show_types rc)
                               then putStrLn$ "\nTypes\n\n  " ++ concat (map f as)
                               else return ()
