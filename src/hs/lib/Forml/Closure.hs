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

module Forml.Closure (
    CompilationLevel(..),
    closure,
  ) where

import Control.Applicative
import Control.Monad (MonadPlus, mplus)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error

import System.IO
import System.Environment
import System.Exit (ExitCode(ExitSuccess))
import System.Directory
import System.Process

import Network.HTTP
import Network.URI

import Data.List as L
import Data.URLEncoded
import Data.Maybe (fromMaybe)

data CompilationLevel = Advanced | Simple

infix 1 `guardM`
guardM :: (Error e, Monad m) => m Bool -> String -> ErrorT e m ()
guardM q msg = guard' =<< lift q
  where guard' True = return ()
        guard' False = throwError (strMsg msg)

compilationLevel :: CompilationLevel -> String
compilationLevel Advanced = "ADVANCED_OPTIMIZATIONS"
compilationLevel Simple = "SIMPLE_OPTIMIZATIONS"

closure_local, closure_remote
  :: (Error e) => String -> CompilationLevel -> ErrorT e IO String

closure_local x y =
  do closureEnv <- ErrorT $ maybe (Left $ strMsg "$CLOSURE unset") return
                          . L.lookup "CLOSURE" <$> getEnvironment
     doesFileExist closureEnv `guardM` closureEnv ++ " not found"
     lift $ hFlush stdout >> writeFile "temp.js" x
     retCode <- lift . rawSystem "java" $
                ["-jar", closureEnv,
                 "--compilation_level", compilationLevel y,
                 -- ["--formatting=pretty_print", "--formatting=print_input_delimiter"],
                 "--js", "temp.js", "--js_output_file", "temp.compiled.js",
                 "--warning_level", "QUIET"]
     return (retCode == ExitSuccess) `guardM` "local closure compiler failed"
     doesFileExist "temp.compiled.js" `guardM` "temp.compiled.js not found"
     js <- lift $ readFile "temp.compiled.js"
     length js `seq` 
       lift (removeFile "temp.js" >> removeFile "temp.compiled.js")
     return js

closure_remote x z =
  let uri = fromMaybe undefined $ parseURI "http://closure-compiler.appspot.com/compile"
      y = export$ importList [ ("output_format",     "text")
                             , ("output_info",       "compiled_code")
                             , ("compilation_level", compilationLevel z)
                             , ("js_code",           x) ]
      args = [ mkHeader HdrContentLength (show$ length y)
             , mkHeader HdrContentType "application/x-www-form-urlencoded" ]
  in lift $ simpleHTTP (Request uri POST args y)
            >>= getResponseBody

closure :: (Error e) => Bool -> String -> CompilationLevel -> ErrorT e IO String
closure remote x z =
  closure_local x z `mplus`
    if remote then closure_remote x z
    else throwError (strMsg "Local Closure unavailable; pass -remote option to use public server")
