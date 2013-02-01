{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}

module Forml.Javascript.Test where

import Text.InterpolatedString.Perl6

import Control.Concurrent

import System.Exit
import System.IO
import System.Process
import System.Directory


import Forml.CLI
import Forml.Static

test :: RunConfig -> String -> String -> String -> IO ()
test rc @ RunConfig { run_tests = Node } js title tests =
      let runner = if silent rc then run_silent else monitor in

      runner [qq|Testing {title}.js [Node.js]|] $
      do (Just std_in, Just std_out, _, p) <-
             createProcess (proc "node" []) { std_in = CreatePipe, std_out = CreatePipe }

         forkIO $ do errors <- hGetContents std_out
                     putStr errors
                     hFlush stdout

         hPutStrLn std_in$ jasmine
         hPutStrLn std_in$ js ++ "\n\n"
         hPutStrLn std_in$ tests
         hPutStrLn std_in$ console

         z <- waitForProcess p
    
         case z of
           ExitFailure _ -> return$ Left []
           ExitSuccess   -> return$ Right ()

test rc @ RunConfig { run_tests = Phantom } js title tests =
      let runner = if silent rc then run_silent else monitor in

      runner [qq|Testing {title}.js [Phantom.js]|] $
      do writeFile (output rc ++ ".phantom.js")
               (jquery ++ jasmine ++ js ++ tests ++ console)

         (Just std_in, Just std_out, _, p) <-
             createProcess (proc "phantomjs" [output rc ++ ".phantom.js"]) { std_in = CreatePipe, std_out = CreatePipe }

         forkIO $ do errors <- hGetContents std_out
                     putStr errors
                     hFlush stdout

         z <- waitForProcess p

         removeFile $ output rc ++ ".phantom.js"

         case z of
             ExitFailure _ -> return$ Left []
             ExitSuccess   -> return$ Right ()
             
test rc _ _ _ = if silent rc then do return ()  else warn "Testing" ()
