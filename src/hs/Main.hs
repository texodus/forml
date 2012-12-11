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
import Data.String.Utils (split)

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

    where parse' :: ([Filename], TypeSystem, [(Program, Source)], [Title]) -> String -> IO ([Filename], TypeSystem, [(Program, Source)], [Title])
          parse' (zs, ts, as, titles) src'' =

             do let (title, src) = get_title (head . split "." . last . split "/" . head $ zs) src''
                let src' = to_literate (head zs) . (++ "\n") $ src
                (ts', as') <- monitor [qq|Loading {head zs}|] $ return$ to_parsed (head zs) src' ts
                return (tail zs, ts ++ ts', as ++ [(as', src')], titles ++ [title])

          get_source filename =
             do hFile <- openFile filename ReadMode
                hGetContents hFile

gen_js :: [Source] -> [Program] -> (String, [String])
gen_js src p = (g, h)

    where g = unserialize $ zipWith (render whole_program) src p
          h = map (unserialize . (:[])) $ zipWith (render_spec whole_program) src p

          unserialize x = compress $ read' prelude ++ "\n" ++ (unlines $ map read' x)

          read' xs @ ('"':_) = read xs
          read' x = x
          
          whole_program = Program $ get_program p
          
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

              do (_, types, src', titles) <- parse_forml$ inputs rc
                 let src'' = O.run_optimizer (fmap fst src') types
                 let (js', tests') = gen_js (fmap snd src') src''

               --  writeFile "prelude.types" (B.toString $ S.encode as)

                 js <- if optimize rc
                       then monitor [qq|Closure {output rc}.js |]$ closure_local js' "ADVANCED_OPTIMIZATIONS"
                       else do warn "Closure [libs]" js'

                 tests <- case rc of
                              RunConfig { optimize = True, run_tests = Phantom } ->
                                  zipWithM (\title t -> monitor [qq|Closure {title}.spec.js |]$ closure_local t "SIMPLE_OPTIMIZATIONS") (drop 1 titles) (drop 1 tests')
                              _ -> warn "Closure [tests]" tests'

                 writeFile (output rc ++ ".js") js
                 zipWithM writeFile (map (++ ".spec.js") titles) tests

                 if write_docs rc
                     then let programs = map fst (drop 1 src')
                              sources  = map snd (drop 1 src')
                          in  do docs js tests (drop 1 titles) programs sources
                     else monitor "Docs" $ return $ Right ()

                 sequence (zipWith (test rc js) (drop 1 titles) tests)

                 if (show_types rc)
                      then putStrLn ("\nTypes\n\n  " ++ concat (map f types))
                      else return ()
 