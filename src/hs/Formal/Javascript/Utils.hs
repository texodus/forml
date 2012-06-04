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

module Formal.Javascript.Utils where

import Text.InterpolatedString.Perl6
import Language.Javascript.JMacro
import Data.Monoid
import qualified Data.Map as M
import qualified Data.List as L
import Formal.Parser.Utils
import Text.ParserCombinators.Parsec

import Prelude hiding (curry, (++))


prelude :: JStat
prelude = [jmacro| function !is_array(x) { 
                       return `(InfixExpr "instanceof" x (ref "Array"))`;
                   }

                   function !error(x) { throw x; }

                   function !exhaust() { error("Pattern Match Exhausted"); }

                   function !check(x) {
                       result = (typeof x != "undefined");
                       return result;
                   } |]

instance (ToStat a) => ToStat [a] where
    toStat [] = mempty
    toStat x = foldl1 mappend . map toStat $ x

--------------------------------------------------------------------------------
----
---- Metadata



--------------------------------------------------------------------------------
----
---- Utilities

(++) :: Monoid a => a -> a -> a
(++) = mappend

end (reverse -> x : xs) = x : reverse xs

ref name    = ValExpr (JVar (StrI name))
func var ex = ReturnStat (ValExpr (JFunc [StrI var] (BlockStat [ex])))

declare_this name expr =

    [jmacro| `(declare name expr)`;
             this[`(name)`] = `(ref name)`; |]

declare_window name expr =

    [jmacro| `(declare name expr)`;
             (typeof global == "undefined" ? window : global)[`(name)`] = `(ref name)`; |]

declare name expr =

    [jmacro| `(DeclStat (StrI name) Nothing)`;
             `(ref name)` = `(expr)`; |]

curry 0 jexpr = jexpr
curry n jexpr = func (local_pool $ n - 1) (curry (n - 1) jexpr)

local_pool n = [qq|__{ "abcdefghijklmnopqrstuvqxyz" !! n }__|]

scope x = [jmacroE| (function() { `(x)`; })() |]
