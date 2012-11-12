{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, KindSignatures #-}

module Formal.Javascript.Backend where
        
        import Formal.Types.Namespace
        import Formal.TypeCheck.Types
        import Formal.Parser.Utils 
        
        import Data.Monoid      
        import Data.String.Utils
        import Text.Parsec (sourceLine)
            
        type TypeSystem = [(Namespace, [Assumption])]
        
        data JSState = JSState { src :: String
                               , assumptions :: TypeSystem }
        
        newtype JS a = JS (JSState -> (JSState, a))
        
        instance Monad JS where
          fail x     = JS (\_ -> error$ x) 
          return x   = JS (\y -> (y, x))
          JS f >>= g = JS (\x -> case f x of
                                  (y, x') -> let JS gx = g x'
                                             in  gx y)
         
        instance Functor JS where
            fmap f y = y >>= (\x -> JS (\x' -> (x', f x)))
        
        class Javascript e t | e -> t where toJS :: e -> JS t
        
        runJS :: String -> JS t -> t
        runJS src (JS f) = snd . f $ JSState src []
        
        instance (Javascript a b, Monoid b) => Javascript [a] b where
        
                toJS []     = return mempty
                toJS (x:xs) = do x'  <- toJS x
                                 xs' <- toJS xs
                                 return $ mappend x' xs'
                                 
        get_error :: Addr a -> String -> String
        get_error (Addr (sourceLine -> x) (sourceLine -> y) _) =
        
             rstrip . unlines . lines . rstrip . unlines . take ((y - x) + 1) . drop (x - 1) . lines
     
