{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Forml.Javascript.Utils where


import Text.InterpolatedString.Perl6
import Language.Javascript.JMacro
import Data.Monoid
import Data.String.Utils

import Prelude hiding (curry, (++), error)


prelude :: String
prelude = show $ renderJs 
          [jmacro| function !is_array(x) { 
                       return `(InfixExpr "instanceof" x (ref "Array"))`;
                   }

                   var !__undefined__ = "undefined";

                   function !run(x) { return x(); }

                   function !error(x) { throw x; }

                   function !exhaust() { error("Pattern Match Exhausted"); }

                   function !__check(x, y) {
                       return x.hasOwnProperty(y);
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

end :: forall a. [a] -> [a]
end (reverse -> x : xs) = x : reverse xs
end _ = fail "End is not defined for empty lists"

ref :: String -> JExpr
ref name    = ValExpr (JVar (StrI name))

func :: String -> JStat -> JStat
func var ex = ReturnStat (ValExpr (JFunc [StrI var] (BlockStat [ex])))

declare_this :: forall a. ToJExpr a => [Char] -> a -> JStat
declare_this name expr =

    [jmacro| `(declare (replace " " "_" name) expr)`;
             this[`(replace " " "_" name)`] = `(ref (replace " " "_" name))`; |]

declare_window :: forall a. ToJExpr a => [Char] -> a -> JStat
declare_window name expr =

    [jmacro| `(declare (replace " " "_" name) expr)`;
             (typeof global == "undefined" ? window : global)[`((replace " " "_" name))`] = `(ref (replace " " "_" name))`; |]
declare :: forall a. ToJExpr a => [Char] -> a -> JStat

declare name expr = 
     [jmacro| `(DeclStat (StrI (replace " " "_" name)) Nothing)`;
              `(ref (replace " " "_" name))` = `(expr)`; |]
             
declare_scope :: String -> JExpr -> JStat -> JStat
declare_scope name expr stat =

    BlockStat [ReturnStat (ApplExpr (ValExpr (JFunc [StrI name] stat)) [expr])]

curry :: Int -> (String -> String) -> JStat -> JStat
curry 0 _ jexpr = jexpr
curry n f jexpr = func (f $ local_pool $ n - 1) (curry (n - 1) f jexpr)

local_pool :: Int -> String
local_pool n = [qq|__{ "abcdefghijklmnopqrstuvqxyz" !! n }__|]

scope :: forall a. ToStat a => a -> JExpr
scope x = [jmacroE| (function() { `(x)`; })() |]
