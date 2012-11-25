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

module Forml.Closure where

import Control.Applicative

import System.IO
import System.Environment
import System.Directory
import System.Process

import Network.HTTP
import Network.URI

import Data.List as L
import Data.URLEncoded
import Data.Maybe (fromMaybe)



closure_local :: String -> String -> IO (Either a String)
closure_local x y =

        do env' <- L.lookup "CLOSURE" <$> getEnvironment
           case env' of
             Just env ->
                 do exists' <- doesFileExist env
                    if exists' 
                        then do putStr "[local]"
                                hFlush stdout
                                writeFile "temp.js" x
                                system$ "java -jar $CLOSURE --compilation_level "
                                          ++ y 
                                       --   ++ " --formatting=pretty_print --formatting=print_input_delimiter "
                                          ++ " --js temp.js --warning_level QUIET > temp.compiled.js"
                                js <- readFile "temp.compiled.js"
                                system "rm temp.js"
                                system "rm temp.compiled.js"
                                return $ Right js
                        else putStr " remote]" >> hFlush stdout >> closure x y
             Nothing -> putStr " remote]" >> hFlush stdout >> closure x y



closure :: String -> String -> IO (Either a String)
closure x z = do let uri = fromMaybe undefined $ parseURI "http://closure-compiler.appspot.com/compile" 

                     y = export$ importList [ ("output_format",     "text")
                                            , ("output_info",       "compiled_code")
                                            , ("compilation_level", z)
                                            , ("js_code",           x) ]

                     args = [ mkHeader HdrContentLength (show$ length y)
                            , mkHeader HdrContentType "application/x-www-form-urlencoded" ]

                 rsp <- simpleHTTP (Request uri POST args y)
                 txt <- getResponseBody rsp

                 putStr "[remote]"
                 hFlush stdout

                 return$ Right txt
