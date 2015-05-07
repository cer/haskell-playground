module Main where
 
import TicTacToeAggregate
import Aggregate

import Test.Hspec
 
main :: IO ()
main = hspec $ do
 
  describe "execute" $ do
    it "CreateGame should result in GameCreated" $ do
     seed `execute` CreateGame `shouldBe` Right GameCreated

    it "MakeMove should result in MoveMade" $
      seed <== CreateGame `execute` MakeMove 5 `shouldBe` Right (MoveMade 5)

    it "MakeMove in occupied square should result in error" $
      seed <== CreateGame <== MakeMove 5 `execute` MakeMove 5 `shouldBe` Left NoValidMove


  describe "apply" $ do
  	 it "GameCreated should return game" $ do
  	 	let s = seed in s `apply` GameCreated `shouldBe` s 


  	 it "MoveMade should update Game" $
  	 	s `apply` (MoveMade 5) `shouldBe` s
  	 		where s = seed
 
