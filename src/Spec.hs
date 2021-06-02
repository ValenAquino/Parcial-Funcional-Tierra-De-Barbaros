module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate, AsyncException (ThreadKilled))

correrTests :: IO ()
correrTests = hspec $ do
 
 describe "Punto 1" $ do
   describe "" $ do
      it "" $ do
         2+2 `shouldBe`  4
   
 describe "Punto 2" $ do
    describe "" $ do
      it "" $ do
         2+2 `shouldBe`  4

 describe "Punto 3" $ do
    describe "" $ do
      it "" $ do
         2+2 `shouldBe`  4

 describe "Punto 4" $ do
    describe "" $ do
      it "" $ do
         2+2 `shouldBe`  4

 describe "Punto 4" $ do
    describe "" $ do
      it "" $ do
         2+2 `shouldBe`  4

escribime :: Expectation
escribime = implementame
