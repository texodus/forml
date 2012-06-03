{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Formal.Javascript (render, render_spec) where

import Language.Javascript.JMacro
import Prelude hiding (curry, (++))

import Formal.Types.Statement

import Formal.Parser
import Formal.Javascript.Utils

render :: Program -> String
render (Program xs) = show . renderJs $ ((prelude ++) . toStat . map (empty_meta Library xs) $ xs)

render_spec :: Program -> String
render_spec (Program xs) = show . renderJs . (prelude ++) . wrap . toStat . map (empty_meta Test xs) $ xs
    where wrap x = [jmacro| describe("", function() { `(x)`; }); |]

