{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE ViewPatterns         #-}

module Main(main) where

import Text.InterpolatedString.Perl6

import Control.Concurrent
import Control.Monad.State hiding (lift)

import System.Directory
import System.Environment
import System.IO

import Data.List as L

import           Forml.CLI
import           Forml.Closure
import           Forml.Doc
import           Forml.Javascript
import           Forml.Javascript.Backend
import           Forml.Javascript.Test
import           Forml.Javascript.Utils   (prelude)
import qualified Forml.Optimize           as O
import           Forml.Parser
import           Forml.Static
import           Forml.TypeCheck          hiding (split)
import           Forml.Types.Statement

type Source   = String
type Title    = String
type Error    = String
type Filename = String

to_parsed :: Title -> Source -> TypeSystem -> Either [Error] (TypeSystem, Program)
to_parsed name src env = case parseForml name src of
                              Left x  -> Left [show x]
                              Right x -> case tiProgram x env of
                                           (as, []) -> Right (as, x)
                                           (_, y)   -> Left y

parse_forml :: [Filename] -> IO ([Filename], TypeSystem, [(Program, Source)], [Title])
parse_forml filenames =

    do sources <- mapM get_source filenames
       foldM parse' ("prelude.forml" : filenames, [], [], []) (prelude' : sources)

    where parse' :: ([String], TypeSystem, [(Program, String)], [String]) -> String -> IO ([String], TypeSystem, [(Program, String)], [String])
          parse' (zs, ts, as, titles) src'' =

             do let (title, src) = get_title src''
                let src' = to_literate (head zs) . (++ "\n") $ src
                (ts', as') <- monitor [qq|Loading {head zs}|] $ return$ to_parsed (head zs) src' ts
                return (tail zs, ts ++ ts', as ++ [(as', src')], titles ++ [title])

          get_source filename =
             do hFile <- openFile filename ReadMode
                hGetContents hFile

gen_js :: [String] -> [Program] -> (String, [(String, String)])
gen_js src p = (compress (read' prelude ++ "\n" ++ (unlines $ map read' $ zipWith (render (Program $ get_program p)) src p)), [("",  read' prelude ++ "\n" ++ (unlines $ map read' $ zipWith (render_spec (Program $ get_program p)) src p))])


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
                       then (monitor "Closure [libs"$ closure_local js' "ADVANCED_OPTIMIZATIONS")
                       else do warn "Closure [libs]" js'

                 tests <- case rc of
                              RunConfig { optimize = True, run_tests = Phantom } ->
                                  monitor "Closure [tests"$ closure_local tests' "SIMPLE_OPTIMIZATIONS"
                              _ -> warn "Closure [tests]" tests'

                 writeFile (output rc ++ ".js") js
                 writeFile (output rc ++ ".spec.js") tests

                 monitor "Docs" $
                     if write_docs rc
                     then let xxx = map fst src'
                              yyy = map snd src'
                              html' xxx' yyy' = highlight (case xxx' of (Program xs) -> get_tests xs)$ toHTML (annotate_tests yyy' xxx')
                              html = concat $ zipWith html' xxx yyy
                              compiled = [qq|<script>$js $tests</script>|]
                              hook = "<script>" ++ htmljs ++ ";window.document.title='"++(titles !! 0)++"';$('h1').html('" ++ (titles !! 0) ++ "')</script>"

                          in do writeFile ((output rc) ++ ".html") (header ++ css' ++ scripts ++ compiled ++ html ++ hook ++ footer)
                                return $ Right ()

                     else return $ Right ()

                 case run_tests rc of
                     Node    -> test_node rc js tests
                     Phantom -> test_phantom rc js tests
                     NoTest  -> warn "Testing" ()

                 if (show_types rc)
                      then putStrLn ("\nTypes\n\n  " ++ concat (map f as))
                      else return ()

