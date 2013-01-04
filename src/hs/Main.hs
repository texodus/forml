{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}

module Main(main) where

import Text.InterpolatedString.Perl6

import Control.Concurrent
import Control.Monad.State hiding (lift)

import System.Directory
import System.Environment
import System.IO
import System.IO.Unsafe


import Data.String.Utils (split)
import Data.List as L
import qualified Data.Serialize as S
import qualified Data.ByteString as B

import GHC.Generics

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
import           Forml.TypeCheck

to_parsed :: Title -> Source -> TypeSystem -> Either [Error] (TypeSystem, Program)
to_parsed name src env = case parseForml name src of
                              Left x  -> Left [show x]
                              Right x -> case tiProgram x env of
                                           (as, []) -> Right (as, x)
                                           (_, y)   -> Left y

to_filename = head . split "." . last . split "/"

data Compiled = Compiled { filename :: Filename
                         , types    :: TypeSystem
                         , program  :: Program
                         , source   :: Source
                         , title    :: Title
                         , js       :: String
                         , opt_st   :: O.OptimizeState
                         , tests    :: String } deriving (Generic)

instance S.Serialize Compiled

db :: Show a => a -> a
db x = unsafePerformIO $ do putStrLn$ "-- " ++ (show x)
                            return x



parse_forml :: [Filename] -> Compiled -> IO [Compiled]
parse_forml filenames c' =

    do sources <- mapM get_source filenames
       foldM parse'
             [c']
             (sources `zip` filenames)

    where parse' :: [Compiled]
                 -> (Source, Filename)
                 -> IO [Compiled]

          parse' acc (src'', filename) = do

              let Compiled { types = ts, opt_st = opt } = last acc
              let (title, src) = get_title (to_filename filename) src''
              let src'         = to_literate filename . (++ "\n") $ src

              (ts', ast) <- monitor [qq|Loading {filename}|] $ return $ to_parsed filename src' ts

              let (opt', opt_ast) = O.run_optimizer ast (opt { O.assumptions = ts'})
              let (js',  tests')  = gen_js src' (opt_ast) (whole_program $ map program acc ++ [opt_ast])

              return $ acc ++ [Compiled (to_filename filename) ts' opt_ast src' title js' opt' tests']

          get_source filename =
             do hFile <- openFile filename ReadMode
                hGetContents hFile

          whole_program p = Program $ get_program p
          
          get_program (Program ss: ps) = ss ++ get_program ps
          get_program [] = []


gen_js :: Source -> Program -> Program -> (String, String)
gen_js src p whole_program = (g, h)

    where g = unserialize $ render whole_program src p
          h = unserialize $ render_spec whole_program src p

          unserialize x = compress $ read' x

read' xs @ ('"':_) = read xs
read' x = x

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

              do compiled <- drop (implicit_prelude rc)
                     `fmap` parse_forml (inputs rc) (Compiled "" [] (Program []) "" "" [] (O.gen_state []) [])
                  
                 _ <- mapM (\ c @ (Compiled { .. }) -> B.writeFile (filename ++ ".obj") $ S.encode c)
                           (drop (2 - implicit_prelude rc) compiled)

                 js' <- ((read' prelude ++ "\n") ++) `fmap`
                        if optimize rc
                        then monitor [qq|Closure {output rc}.js |]$ closure_local (js . last $ compiled) "ADVANCED_OPTIMIZATIONS"
                        else do warn "Closure [libs]" (js . last $ compiled)

                 tests' <- case rc of
                              RunConfig { optimize = True } ->
                                  zipWithM (\title t -> monitor [qq|Closure {title}.spec.js |]$ closure_local t "SIMPLE_OPTIMIZATIONS")
                                               (map filename compiled)
                                               (map tests compiled)
                              _ -> warn "Closure [tests]" (map tests compiled)

                 writeFile (output rc ++ ".js") js'
                 _ <- zipWithM writeFile (map (++ ".spec.js") (map filename compiled)) tests'

                 if write_docs rc
                     then docs js'
                              tests'
                              (map filename compiled)
                              (map title compiled)
                              (map program compiled)
                              (map source compiled)
                     else monitor "Docs" $ return $ Right ()

                 _ <- sequence (zipWith (test rc js') (map filename compiled) tests')

                 if (show_types rc)
                      then putStrLn ("\nTypes\n\n  " ++ concatMap (concatMap f . types) compiled)
                      else return ()
 