module Main(main) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import Control.Monad.Variables

import Control.Concurrent.STM
import Control.Monad.ST hiding (unsafeSTToIO)
import Control.Monad.ST.Unsafe(unsafeSTToIO)

import Control.Monad.Reader
import Control.Monad.Cont

baseTests :: (MonadVar m) => (m Int -> IO Int) -> Spec
baseTests run = do
  it "supports variable creation" $ do
    result <- run $ do
      var <- newVar 2
      readVar var
    result `shouldBe` 2

  it "supports variable modification" $ do
    result <- run $ do
      var <- newVar 2
      writeVar var 3
      readVar var
    result `shouldBe` 3

main :: IO ()
main = hspec $ do
  describe "the IO instance" $ baseTests id
  describe "the STM instance" $ baseTests atomically
  describe "the ST instance" $ baseTests unsafeSTToIO
  describe "the ReaderT lifted instance" $ baseTests $ (`runReaderT` ())
  describe "the ContT lifted instance" $ baseTests $ (`runContT` return)

