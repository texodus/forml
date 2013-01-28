{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE ViewPatterns         #-}

module Forml.Static where

import Text.InterpolatedString.Perl6

import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import           Data.FileEmbed
import           Data.Monoid

import Control.Arrow

import qualified Codec.Compression.GZip as G


console  = "$prelude.$html.console_runner()"

find key =
    maybe (error key) id (lookup key statics)
    where
        conv = fmap (second B.toString)
        statics = 
            conv $(embedDir "lib") 
            ++ conv $(embedDir "src/html")
            ++ conv $(embedDir "src/css")
            ++ conv $(embedDir "src/js")


prelude' = BS.concat . BL.toChunks . G.decompress $ BL.fromChunks [$(embedFile "prelude.obj")]

jquery   = find "js/jquery.js"
report   = find "FormlReporter.js"

html_template = find "template.html"

scripts :: String
scripts  =  [qq|<script>$jasmine $report $prettify</script>|]


jasmine  = find "js/jasmine-1.0.1/jasmine.js" 
               `mappend` find "js/jasmine-1.0.1/jasmine-html.js"

 
prettify = find "js/prettify.js"
               `mappend` find "js/lang-hs.js"
               

css      = find "css/jasmine.css"  
               `mappend` find "css/prettify.css"
               `mappend` find "styles.css"

css' :: String
css' = [qq|<style type=\"text/css\">$css</style>|]

