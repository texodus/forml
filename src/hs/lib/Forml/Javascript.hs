{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             MultiParamTypeClasses, NamedFieldPuns, OverlappingInstances,
             QuasiQuotes, RankNTypes, RecordWildCards, TypeSynonymInstances,
             UndecidableInstances #-}

module Forml.Javascript (render, render_spec) where

import Prelude hiding (curry, (++))

import qualified Data.Map as M

import Language.Javascript.JMacro

import Forml.Javascript.Backend
import Forml.Types.Statement
import Forml.Parser


render :: Program -> String -> Program -> String
render (Program ys) src (Program xs) =
    show . renderJs . runJS src . toJS . map (empty_meta Library ys) $ xs

render_spec :: Program -> String -> Program -> String
render_spec (Program ys) src (Program xs) =
    show . renderJs . wrap . runJS src . toJS . map (empty_meta Test ys) $ xs
    where wrap x = [jmacro| describe("", function() { `(x)`; }); |]



