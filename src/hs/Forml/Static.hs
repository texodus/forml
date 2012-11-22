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
import           Data.FileEmbed
import           Data.Monoid


jasmine  :: String
header   :: String
css      :: String
prelude' :: String
jquery   :: String
footer   :: String
report   :: String
prettify :: String
htmljs   :: String
console  :: String
scripts  :: String
css'     :: String

htmljs   = "$('code').addClass('prettyprint lang-hs');prettyPrint();$('#run_tests').bind('click', prelude.html.table_of_contents)"
console  = "prelude.html.console_runner()"

prelude' = B.toString $(embedFile "src/forml/prelude.forml")
jquery   = B.toString $(embedFile "lib/js/jquery.js")
header   = B.toString $(embedFile "src/html/header.html")
footer   = B.toString $(embedFile "src/html/footer.html")
report   = B.toString $(embedFile "src/js/FormlReporter.js")

scripts  = [qq|<script>$jquery $jasmine $report $prettify</script>|]
css'     = [qq|<style type=\"text/css\">$css</style>|]


jasmine  = B.toString $(embedFile "lib/js/jasmine-1.0.1/jasmine.js") 
               `mappend` B.toString $(embedFile "lib/js/jasmine-1.0.1/jasmine-html.js")

prettify = B.toString $(embedFile "lib/js/prettify.js") 
               `mappend` B.toString $(embedFile "lib/js/lang-hs.js")

css      = B.toString $(embedFile "lib/js/jasmine-1.0.1/jasmine.css") 
               `mappend` B.toString $(embedFile "src/html/styles.css") 
               `mappend` B.toString $(embedFile "lib/js/prettify.css")
