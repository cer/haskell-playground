{-# LANGUAGE TypeFamilies #-}

module TicTacToeAggregate(execute, seed, apply, Command(..), Event(..), Error(..)) where 

import Aggregate
import TicTacToe
import Data.Either
import Data.Maybe

data Game = Game {
	state :: GameState
} deriving (Show, Eq)

instance Aggregate Game where

	data Error Game = NoValidMove deriving (Show, Eq)

	data Event Game = GameCreated
	                 | MoveMade Int
					 | GameWon
					 | GameTied  deriving (Show, Eq)

	data Command Game = CreateGame | MakeMove Int  deriving (Show, Eq)

	_ `execute` CreateGame = Right GameCreated
	
	Game state `execute` MakeMove k = 
		case makeMove k state of
			Nothing -> Left NoValidMove
			Just _ -> Right (MoveMade k)
		

	state `apply` GameCreated = state
	
	s `apply` MoveMade k =  s { state = fromJust $ makeMove k (state s) }

	state `apply` GameWon = state
	state `apply` GameTied = state

	seed = Game initialGameState



