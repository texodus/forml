module Forml.CLI where

import Control.Monad.State hiding (lift)

import System.Console.ANSI
import System.Exit
import System.IO
import System.Process

import Data.String.Utils


data TestMode  = NoTest | Node | Phantom

data RunConfig = RunConfig { inputs :: [String]
                           , output :: String
                           , show_types :: Bool
                           , optimize :: Bool
                           , silent :: Bool
                           , flush :: Bool
                           , run_tests :: TestMode
                           , write_docs :: Bool
                           , implicit_prelude :: Bool
                           , watch :: Bool }


parseArgs :: [String] -> RunConfig
parseArgs = fst . runState argsParser
  where argsParser = do args <- get
                        case args of
                          []      -> return $ RunConfig [] "default" False True False False Phantom False True False
                          (x':xs) -> do put xs
                                        case x' of
                                          "-w"          -> do x <- argsParser
                                                              return $ x { watch = True }
                                          "-docs"       -> do x <- argsParser
                                                              return $ x { write_docs = True }
                                          "-t"          -> do x <- argsParser
                                                              return $ x { show_types = True }
                                          "-no-prelude" -> do x <- argsParser
                                                              return $ x { implicit_prelude = False }
                                          "-no-opt"     -> do x <- argsParser
                                                              return $ x { optimize = False }
                                          "-silent"     -> do x <- argsParser
                                                              return $ x { silent = True }
                                          "-flush"      -> do x <- argsParser
                                                              return $ x { flush = True }
                                          "-no-test"    -> do x <- argsParser
                                                              return $ x { run_tests = NoTest }
                                          "-node-test"  -> do x <- argsParser
                                                              return $ x { run_tests = Node }
                                          "-o"          -> do (name:ys) <- get
                                                              put ys
                                                              RunConfig a _ c d e f g h i j <- argsParser
                                                              return $ RunConfig a name c d e f g h i j
                                          ('-':_)       -> error "Could not parse options"
                                          z             -> do RunConfig a _ c d e f g h i j <- argsParser
                                                              let b = last $ split "/" $ head $ split "." z
                                                              return $ RunConfig (x':a) b c d e f g h i j

type StatusLogger a = String -> a -> IO a

status_logger :: [SGR] -> String -> StatusLogger a
status_logger sgrs rep = 
  let logger str out =  
        colors ((putStr $ "[" ++ rep ++ "] " ++ str) >> return out) $
        do  putStr "\r["
            setSGR sgrs
            putStr rep
            setSGR []
            putStrLn$ "] " ++ str
            return out in
  logger

success :: String -> a -> IO a
success = status_logger [SetColor Foreground Dull Green] "*"

warn :: String -> a -> IO a
warn = status_logger [SetColor Foreground Dull Yellow] "-"

failure :: String -> a -> IO a
failure = status_logger [SetColor Foreground Dull Red] "X"

colors :: IO a -> IO a -> IO a
colors failure success =
          do (_, Just std_out', _, p) <-
                 createProcess (shell "tput colors 2> /dev/null") { std_out = CreatePipe }
             waitForProcess p
             c <- hGetContents std_out'
             case reads (strip c) of
                [(x, "")] | x > (2 :: Integer) -> success
                _ -> failure

type Runner a = String -> IO (Either [String] a) -> IO a

run_silent :: Runner a
run_silent _ d = 
  do  d' <- d
      case d' of 
        Right y -> do return y
        Left  y -> do exitFailure

monitor :: Runner a
monitor x d = do colors (return ()) $ putStr $ "[ ] " ++ x
                 hFlush stdout
                 d' <- d
                 case d' of
                   Right y -> success x y
                   Left  y -> do  failure x y 
                                  if length y <= 5
                                    then mapM putStrLn y >> return ()
                                    else mapM putStrLn (take 5 y) >> putStrLn ("\n" ++ show (length y - 5) ++ " additional errors")
                                  exitFailure