{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}

module Main(main) where

import           Text.InterpolatedString.Perl6

import           Control.Concurrent
import           Control.Monad.State           hiding (lift)

import           System.Directory
import           System.Environment
import           System.IO
import           System.IO.Unsafe
import           System.Log.Handler.Syslog
import           System.Log.Logger

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           Data.List                     as L
import qualified Data.Serialize                as S
import           Data.String.Utils             (split)

import           GHC.Generics

import           Forml.CLI
import           Forml.Closure
import           Forml.Doc
import           Forml.Javascript
import           Forml.Javascript.Backend
import           Forml.Javascript.Test
import           Forml.Javascript.Utils        (prelude)
import qualified Forml.Optimize                as O
import           Forml.Optimize.Optimizer      as OP
import           Forml.Parser
import           Forml.Static
import           Forml.TypeCheck

import qualified Codec.Compression.GZip        as G

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
                         , opt_st   :: OptimizeState
                         , tests    :: String } deriving (Generic)

instance S.Serialize Compiled

parse_forml :: [Filename] -> Compiled -> Runner (TypeSystem, Program) -> IO [Compiled]
parse_forml filenames compiled runner =

    do sources <- mapM get_source filenames
       foldM parse'
             [compiled]
             (sources `zip` filenames)

    where parse' :: [Compiled]
                 -> (Source, Filename)
                 -> IO [Compiled]

          parse' acc (src'', filename) = do

              let Compiled { types = ts, opt_st = opt } = last acc
              let (title, src) = get_title (to_filename filename) src''
              let src'         = to_literate filename . (++ "\n") $ src

              (ts', ast) <- runner [qq|Loading {filename}|] $ return $ to_parsed filename src' ts

              let (opt', opt_ast) = run_optimizer ast (opt { OP.assumptions = ts'})
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
main  = do  args <- getArgs
            if silent $ parseArgs args
              then updateGlobalLogger "Global" (setLevel ERROR)
              else updateGlobalLogger "Global" (setLevel INFO)
            main' $ parseArgs args

main' :: RunConfig -> IO ()
main' rc' =
    if watch rc'
      then watch' rc'
      else compile rc'

    where f (x, y) = show x ++ "\n    " ++ concat (L.intersperse "\n    " (map show y)) ++ "\n\n  "

          runner = if silent rc' then run_silent else monitor

          watch' rc =
              do  x <- mapM getModificationTime . inputs $ rc
                  compile rc
                  infoM "Global" "Waiting ..."
                  wait rc x

          wait rc x =
              do threadDelay 1000
                 x' <- mapM getModificationTime . inputs $ rc
                 if x /= x' then do infoM "Global" "\r"
                                    watch' rc
                            else wait rc x

          compile rc =
              let empty_state =
                      Compiled "" [] (Program []) "" "" [] (OP.gen_state []) [] in

              do state <- if implicit_prelude rc
                          then return $ case S.decode prelude' of
                              Left x -> error x
                              Right x -> x
                          else return $ empty_state

                 compiled <- drop 1 `fmap` parse_forml (inputs rc) state runner

                 _ <- mapM (\ c @ (Compiled { .. }) ->
                                runner [qq|Compiling {filename}.obj |] $ fmap Right $
                                B.writeFile (filename ++ ".obj") $ B.concat $ BL.toChunks $ G.compress $ BL.fromChunks [S.encode c])
                           compiled

                 let js'' = read' prelude ++ "\n"
                               ++ if implicit_prelude rc then js state ++ (concatMap js compiled) else concatMap js compiled

                 js' <- case rc of
                              RunConfig { optimize = True } ->
                                runner [qq|Closure {output rc}.js |] $ closure_local js'' "ADVANCED_OPTIMIZATIONS"
                              RunConfig { silent = False } ->
                                do warn "Closure [libs]" js''
                              _ -> do return js''

                 tests' <- case rc of
                              RunConfig { optimize = True } ->
                                  zipWithM (\title t -> runner [qq|Closure {title}.spec.js |] $ closure_local t "SIMPLE_OPTIMIZATIONS")
                                      (map filename compiled)
                                      (map ((read' prelude ++) . tests) compiled)
                              RunConfig { silent = False } ->
                                  warn "Closure [tests]" (map tests compiled)
                              _ -> do return (map tests compiled)

                 if flush rc
                    then putStr js' >> hFlush stdout
                    else writeFile (output rc ++ ".js") js'

                 _ <- zipWithM writeFile (map (++ ".spec.js") (map filename compiled)) tests'

                 if write_docs rc
                     then docs js'
                              tests'
                              (map filename compiled)
                              (map title compiled)
                              (map program compiled)
                              (map source compiled)
                     else runner "Docs" $ return $ Right ()

                 _ <- sequence (zipWith (test rc js') (map filename compiled) tests')

                 if (show_types rc)
                      then putStrLn $ ("\nTypes\n\n  " ++ concatMap (concatMap f . types) compiled)
                      else return ()

