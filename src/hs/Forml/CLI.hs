
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
                           , run_tests :: TestMode
                           , write_docs :: Bool
                           , implicit_prelude :: Bool
                           , watch :: Bool }


parseArgs :: [String] -> RunConfig
parseArgs = fst . runState argsParser

  where argsParser = do args <- get
                        case args of
                          []     -> return $ RunConfig [] "default" False True Phantom False True False
                          (x':xs) -> do put xs
                                        case x' of
                                         "-w"    -> do x <- argsParser
                                                       return $ x { watch = True }
                                         "-docs"    -> do x <- argsParser
                                                          return $ x { write_docs = True }
                                         "-t"    -> do x <- argsParser
                                                       return $ x { show_types = True }
                                         "-no-prelude" -> do x <- argsParser
                                                             return $ x { implicit_prelude = False }
                                         "-no-opt" -> do x <- argsParser
                                                         return $ x { optimize = False }
                                         "-no-test" -> do x <- argsParser
                                                          return $ x { run_tests = NoTest }
                                         "-node-test" -> do x <- argsParser
                                                            return $ x { run_tests = Node }
                                         "-o"    -> do (name:ys) <- get
                                                       put ys
                                                       RunConfig a _ c d e f g h <- argsParser
                                                       return $ RunConfig a name c d e f g h
                                         ('-':_) -> error "Could not parse options"
                                         z       -> do RunConfig a _ c d e f g h <- argsParser
                                                       let b = last $ split "/" $ head $ split "." z

                                                       return $ RunConfig (x':a) b c d e f g h



warn :: String -> a -> IO a
warn x js = colors ((putStr $ "[-] " ++ x) >> return js) $
            do putStr "\r["
               setSGR [SetColor Foreground Dull Yellow]
               putStr "-"
               setSGR []
               putStrLn$ "] " ++ x
               return js

colors :: IO a -> IO a -> IO a
colors failure success =

          do (_, Just std_out', _, p) <-
                 createProcess (shell "tput colors 2> /dev/null") { std_out = CreatePipe }
             waitForProcess p
             c <- hGetContents std_out'
             case reads (strip c) of
                [(x, "")] | x > (2 :: Integer) -> success
                _ -> failure



monitor :: String -> IO (Either [String] a) -> IO a
monitor x d = do colors (return ()) $ putStr $ "[ ] " ++ x
                 hFlush stdout
                 d' <- d
                 case d' of
                   Right y -> colors ((putStrLn $ "[*] " ++ x) >> return y) $
                              do putStr "\r["
                                 setSGR [SetColor Foreground Dull Green]
                                 putStr "*"
                                 setSGR []
                                 putStrLn$ "] " ++ x
                                 return y
                   Left y  -> colors (errors y) $
                              do putStr "\r["
                                 setSGR [SetColor Foreground Dull Red]
                                 putStr "X"
                                 setSGR []
                                 putStrLn$ "] " ++ x
                                 putStrLn ""
                                 if length y <= 5
                                    then mapM putStrLn y >> return ()
                                    else mapM putStrLn (take 5 y) >> putStrLn ("\n" ++ show (length y - 5) ++ " additional errors")
                                 exitFailure

     where errors y =
                  do putStrLn ("[X] " ++ x)
                     if length y <= 5
                        then mapM putStrLn y >> return ()
                        else mapM putStrLn (take 5 y) >> putStrLn ("\n" ++ show (length y - 5) ++ " additional errors")
                     exitFailure

