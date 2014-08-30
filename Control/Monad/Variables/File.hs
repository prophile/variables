module Control.Monad.Variables.File(file) where

import Control.Monad.Variables
import Control.Monad.IO.Class
import qualified Data.ByteString as BS

file :: (MonadIO m) => FilePath -> Variable m BS.ByteString
file path = Variable { readVar = liftIO (BS.readFile path),
                       writeVar = liftIO . BS.writeFile path }

