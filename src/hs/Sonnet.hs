
module Main where

import Control.Applicative
import Control.Monad.State

import System.IO

import System.Environment

import Sonnet.Parser
import Sonnet.TypeCheck
import Sonnet.Javascript
import Sonnet.Util



--------------------------------------------------------------------------------
--
-- Main

main :: IO ()
main  = do RunConfig inputs output runMode <- parseArgs <$>getArgs
           let name = head inputs
           hFile  <- openFile name ReadMode
           src    <- trim <$> hGetContents hFile
           case parseSonnet src of
             Left ex -> putStrLn $ show ex
             Right s -> let eqs1 = typeCheck $ s
                            eqs  = unify eqs1
                        in  case verify eqs of
                          Left x  -> putStrLn (concat x)
                          Right x -> do case length x of
                                          0 -> return ()
                                          _ -> putStrLn (trim $ concat x)
                                        case runMode of
                                          Compile -> let code = render s
                                                         tests = renderTests output s
                                                     in  do writeFile (output ++ ".js") code
                                                            writeFile (output ++ ".spec.js") tests
                                                            writeFile (output ++ ".html") (genHTML output)
                                          _______ -> do putStrLn $ concat (map ((++ "\n") . show) eqs1)
                                                        putStrLn $ concat (map ((++ "\n") . show) eqs)

data RunMode   = Compile | JustTypeCheck
data RunConfig = RunConfig [String] String RunMode

parseArgs :: [String] -> RunConfig
parseArgs = fst . runState argsParser

  where argsParser = do args <- get
                        case args of
                          []     -> return $ RunConfig [] "default.js" Compile
                          (x:xs) -> do put xs
                                       case x of
                                         ('-':'t':[]) -> do RunConfig a b _ <- argsParser
                                                            return $ RunConfig (x:a) b JustTypeCheck
                                         ('-':'o':[]) -> do (name:ys) <- get
                                                            put ys
                                                            RunConfig a _ c <- argsParser
                                                            return $ RunConfig (x:a) name c
                                         ('-':_) -> error "Could not parse options"
                                         _ -> do RunConfig a b c <- argsParser
                                                 return $ RunConfig (x:a) b c

genHTML :: String -> String
genHTML name = "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN' 'http://www.w3.org/TR/html4/loose.dtd'>"
               ++ "<html><head><title>" ++ name ++ " test suite</title><link rel='stylesheet' type='text/css' href='lib/js/jasmine-1.0.1/jasmine.css'>"
               ++ "<script type='text/javascript' src='lib/js/jasmine-1.0.1/jasmine.js'></script>"
               ++ "<script type='text/javascript' src='lib/js/jasmine-1.0.1/jasmine-html.js'></script>"
               ++ "<script type='text/javascript' src='lib/js/zepto.js'></script>"
               ++ "<script type='text/javascript' src='" ++ name ++ ".js'></script>"
               ++ "<script type='text/javascript' src='" ++ name ++ ".spec.js'></script>"
               ++ "</head>"
               ++ "<body>"
               ++ "<script type='text/javascript'>"
               ++ "jasmine.getEnv().addReporter(new jasmine.TrivialReporter());"
               ++ "jasmine.getEnv().execute();"
               ++ "</script>"
               ++ "</body>"
               ++ "</html>"

