{-# LANGUAGE RankNTypes #-}

module Control.Monad.Variables.Lens(through) where

import Control.Monad.Variables
import Control.Applicative
import Control.Monad
import Data.Functor.Identity

through :: (Monad m) => Variable m a -> (forall f. Functor f => (b -> f b) -> (a -> f a)) -> Variable m b
through v l = Variable { readVar = readVar',
                         writeVar = writeVar' }
  where readVar'     = liftM (getConst . l Const) (readVar v)
        writeVar' v' = do previousValue <- readVar v
                          let newValue = runIdentity $ l (const (Identity v')) previousValue
                          writeVar v newValue

