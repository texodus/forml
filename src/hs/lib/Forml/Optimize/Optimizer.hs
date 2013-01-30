
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Forml.Optimize.Optimizer where
import           System.IO.Unsafe      ()

import           Control.Applicative
import           Control.Monad

import           Data.Serialize

import           Forml.Optimize.Inline
import           Forml.Parser.Utils
import           Forml.TypeCheck.Types hiding (get_namespace)
import           Forml.Types.Namespace hiding (Module)

import           Prelude               hiding (curry)

import           GHC.Generics

data OptimizeState = OptimizeState { ns          :: Namespace
                                   , assumptions :: [(Namespace, [Assumption])]
                                   , inlines     :: Inlines
                                   , tco         :: [String]
                                   , env         :: Inline } deriving (Eq, Generic)

data Optimizer a = Optimizer (OptimizeState -> (OptimizeState, a))

instance Serialize OptimizeState

instance Monad Optimizer where

    fail   x = Optimizer (\_ -> error x)
    return x = Optimizer (\y -> (y, x))

    Optimizer f >>= g =
        Optimizer (\x -> case f x of (y, x) -> let Optimizer gx = g x in gx y)

instance Functor Optimizer where

    fmap f (Optimizer g) = Optimizer (\x -> case g x of (y, x) -> (y, f x))

instance Applicative Optimizer where

    pure = return
    x <*> y = x >>= flip fmap y

class Optimize a where

    optimize :: a -> Optimizer a

set_namespace :: Namespace -> Optimizer ()
set_namespace ns' = Optimizer (\x -> (x { ns = ns' }, ()))

get_namespace :: Optimizer Namespace
get_namespace  = Optimizer (\x -> (x, ns x))

set_inline :: Inlines -> Optimizer ()
set_inline ns' = Optimizer (\x -> (x { inlines = ns' }, ()))

get_inline :: Optimizer Inlines
get_inline  = Optimizer (\x -> (x, inlines x))

set_env :: Inline -> Optimizer ()
set_env ns' = Optimizer (\x -> (x { env = ns' }, ()))

get_env :: Optimizer Inline
get_env  = Optimizer (\x -> (x, env x))

add_tco :: String -> Optimizer ()
add_tco x = Optimizer (\y -> (y { tco = x : tco y }, ()))

with_env :: forall b. Optimizer b -> Optimizer b
with_env xs =

    do e <- get_env
       xs' <- xs
       set_env e
       return xs'

gen_state as = OptimizeState (Namespace []) as [] [] []

run_optimizer :: (Optimize a) => a -> OptimizeState -> (OptimizeState, a)
run_optimizer p @ (optimize -> Optimizer f) as = f as


instance (Optimize a) => Optimize (Maybe a) where

    optimize (Just x) = Just <$> optimize x
    optimize Nothing  = return Nothing

instance (Optimize a) => Optimize (Addr a) where

    optimize (Addr s e a) = Addr s e <$> optimize a



