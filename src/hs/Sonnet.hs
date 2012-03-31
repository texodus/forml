
module Main where

import Text.Pandoc
import Data.Char (ord, isAscii)
import Data.String.Utils
import qualified Data.List as L

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

-- Utils
-----------------------------------------------------------------------------------



toEntities :: 
-- Convert a string w/unicode characters to a string encoded in HTML entity format.

           String            -> String
toEntities []                 = ""
toEntities (c:cs) | isAscii c = c : toEntities cs
                  | otherwise = "&#" ++ show (ord c) ++ ";" ++ toEntities cs


-- Converts a string in Markdown format to a string in HTML format.
toHTML :: String -> String
toHTML = toEntities . writeHtmlString defaultWriterOptions . readMarkdown defaultParserState


-- Converts a string in markdown format to a string with all non-code blocks
-- removed.
toCode :: String -> String
toCode = concat . L.intersperse "\n" . L.filter isCode . lines

    where isCode (' ':' ':' ':' ':_) = True
          isCode _                   = False




main :: IO ()
main  = do RunConfig inputs output runMode <- parseArgs <$> getArgs
           putStrLn output
           let file = head inputs
           hFile   <- openFile file ReadMode
           raw_src <- hGetContents hFile
           let src = toCode . trim $ raw_src
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
                                                         docs  = toHTML raw_src
                                                     in  do writeFile (output ++ ".js") code
                                                            writeFile (output ++ ".spec.js") tests
                                                            writeFile (output ++ ".html") (wrap_html output docs)
                                          _______ -> do putStrLn $ concat (map ((++ "\n") . show) eqs1)
                                                        putStrLn $ concat (map ((++ "\n") . show) eqs)

data RunMode   = Compile | JustTypeCheck
data RunConfig = RunConfig [String] String RunMode

parseArgs :: [String] -> RunConfig
parseArgs = fst . runState argsParser

  where argsParser = do args <- get
                        case args of
                          []     -> return $ RunConfig [] "default" Compile
                          (x:xs) -> do put xs
                                       case x of
                                         ('-':'t':[]) -> do RunConfig a b _ <- argsParser
                                                            return $ RunConfig (x:a) b JustTypeCheck
                                         ('-':'o':[]) -> do (name:ys) <- get
                                                            put ys
                                                            RunConfig a _ c <- argsParser
                                                            return $ RunConfig (x:a) name c
                                         ('-':_) -> error "Could not parse options"
                                         z -> do RunConfig a _ c <- argsParser
                                                 let b = last $ split "/" $ head $ split "." z
                                                 return $ RunConfig (x:a) b c



-- Docs

wrap_html :: String -> String -> String
wrap_html name body =

    "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN' 'http://www.w3.org/TR/html4/loose.dtd'>"
        ++ "<html><head><title>" ++ name ++ "</title>"
        ++ "<link rel='stylesheet' type='text/css' href='lib/js/jasmine-1.0.1/jasmine.css'>"
        ++ "<script type='text/javascript' src='lib/js/jasmine-1.0.1/jasmine.js'></script>"
        ++ "<script type='text/javascript' src='lib/js/jasmine-1.0.1/jasmine-html.js'></script>"
        ++ "<script type='text/javascript' src='lib/js/zepto.js'></script>"
        ++ "<script type='text/javascript' src='" ++ name ++ ".js'></script>"
        ++ "<script type='text/javascript' src='" ++ name ++ ".spec.js'></script>"
        ++ "<link href='http://kevinburke.bitbucket.org/markdowncss/markdown.css' rel='stylesheet'></link>"
        ++ "<link href='lib/js/prettify.css' type='text/css' rel='stylesheet' />"
        ++ "<script type='text/javascript' src='lib/js/prettify.js'></script>"
        ++ "<script type='text/javascript' src='lib/js/lang-hs.js'></script>"
        ++ "<script type='text/javascript' src='lib/js/jquery.js'></script>"
        ++ "<style>ul{padding-left:40px;}</style>"
        ++ "</head>"
        ++ "<body><div style='margin: 0 0 50px 0'>"
        ++ body
        ++ "</div><script type='text/javascript'>"
        ++ "jasmine.getEnv().addReporter(new jasmine.TrivialReporter());"
        ++ "jasmine.getEnv().execute();"
        ++ "</script>"
        ++ "<script type='text/javascript'>$('code').addClass('prettyprint lang-hs');"
        ++ "prettyPrint()</script>"
        ++ "</body>"
        ++ "</html>"

