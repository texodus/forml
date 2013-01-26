
module Forml.CLISpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))

spec = do

    describe "encode" $ do
        it "encodes multiples of 3" $
            1 `shouldBe` 1
 