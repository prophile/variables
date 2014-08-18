-- | Monads with variables.
module Control.Monad.Variables(Variable(Variable, load, store),
                               MonadVar,
                               stateVar) where

-- From base
import Control.Monad.ST
import Data.IORef
import Data.STRef
import Data.Monoid

-- From mtl
import Control.Monad.Trans
import Control.Monad.State.Class
import Control.Monad.Reader
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS

-- From stm
import Control.Concurrent.STM

-- | The type of variables. In contrast with other "monads-with-variables"
-- packages, this is not done using type families.
data Variable m a = Variable { load  :: m a,
                               store :: a -> m () }

-- | Monads which admit general variables.
class Monad m => MonadVar m where
  -- | Create a variable from an initial value.
  newVar :: a -> m (Variable m a)

-- Base instances
instance MonadVar IO where
  newVar x = do var <- newIORef x
                return Variable { load  = readIORef var,
                                  store = writeIORef var }

instance MonadVar (ST s) where
  newVar x = do var <- newSTRef x
                return Variable { load  = readSTRef var,
                                  store = writeSTRef var }

instance MonadVar STM where
  newVar x = do var <- newTVar x
                return Variable { load  = readTVar var,
                                  store = writeTVar var }

-- Instances for mtl/transformers monad transformers.
liftNewVar :: (MonadTrans t, MonadVar m, Monad (t m)) => a -> t m (Variable (t m) a)
liftNewVar x = do lowerVariable <- lift (newVar x)
                  return Variable { load  = lift (load lowerVariable),
                                    store = lift . store lowerVariable }

instance MonadVar m => MonadVar (ReaderT r m) where
  newVar = liftNewVar

instance (MonadVar m, Monoid w) => MonadVar (WS.WriterT w m) where
  newVar = liftNewVar

instance (MonadVar m, Monoid w) => MonadVar (WL.WriterT w m) where
  newVar = liftNewVar

-- | Access a variable representing the state of a state monad.
stateVar :: MonadState s m => Variable m s
stateVar = Variable { load = get,
                      store = put }

