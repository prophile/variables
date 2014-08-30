{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import Control.Monad.Variables
import Control.Monad.Variables.File
import Control.Monad.Variables.Lens

import Control.Concurrent.STM
import Control.Monad.ST hiding (unsafeSTToIO)
import Control.Monad.ST.Unsafe(unsafeSTToIO)

import Control.Monad.Reader
import Control.Monad.Cont

import Control.Lens

import qualified Data.ByteString as BS

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
  describe "file" $ do
    let fn = "/tmp/variables-var-test"
    let var = file fn
    it "can read files" $ do
      BS.writeFile fn "bees"
      readVar var `shouldReturn` "bees"
    it "can write files" $ do
      writeVar var "gravity"
      BS.readFile fn `shouldReturn` "gravity"
  describe "with lenses" $ do
    it "reads" $ do
      var <- newVar (3, "pony")
      let var' = var `through` _2
      readVar var' `shouldReturn` "pony"
    it "writes" $ do
      var <- newVar (3, "pony")
      let var' = var `through` _2
      writeVar var' "horse"
      readVar var `shouldReturn` (3, "horse")

