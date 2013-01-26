
module MainSpec where

import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck (prop)

import Test.HUnit
import Test.QuickCheck hiding ((.&.), output)

import Forml.Exec
import Forml.CLI

spec = do

    describe "Forml Compiler" $ do
        it "should compile the prelude.forml & the tests suite" $ do
        	main' test_config            

test_config =

	RunConfig { inputs = ["src/forml/prelude.forml", "src/forml/tests.forml"]
              , output = "default.js"
              , show_types = True
              , optimize   = True
              , silent     = False
              , flush      = True
              , run_tests  = NoTest
              , write_docs = True
              , implicit_prelude = False
              , watch      = False }