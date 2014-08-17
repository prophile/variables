module Control.Monad.Variables(Variable(Variable), load, store, MonadVar) where

import Data.IORef

data Variable m a = Variable { load  :: m a,
                               store :: a -> m () }

class Monad m => MonadVar m where
  newVar :: a -> m (Variable m a)

instance MonadVar IO where
  newVar x = do var <- newIORef x
                return Variable { load = readIORef var,
                                  store = writeIORef var }

