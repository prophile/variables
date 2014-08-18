module Main(main) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import Control.Monad.Variables

main :: IO ()
main = hspec $ do
  describe "the IO instance" $ do
    it "supports variable creation" $ do
      var <- newVar 2
      readVar var `shouldReturn` 2

    it "supports variable modification" $ do
      var <- newVar 2
      writeVar var 3
      readVar var `shouldReturn` 3

