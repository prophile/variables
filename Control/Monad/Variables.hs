-- | Monads with variables.
module Control.Monad.Variables(Variable(Variable, load, store), MonadVar) where

import Data.IORef

-- | The type of variables. In contrast with other "monads-with-variables"
-- packages, this is not done using type families.
data Variable m a = Variable { load  :: m a,
                               store :: a -> m () }

-- | Monads which admit general variables.
class Monad m => MonadVar m where
  -- | Create a variable from an initial value.
  newVar :: a -> m (Variable m a)

instance MonadVar IO where
  newVar x = do var <- newIORef x
                return Variable { load = readIORef var,
                                  store = writeIORef var }

