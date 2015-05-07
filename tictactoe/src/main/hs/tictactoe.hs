module TicTacToe(GameState, initialGameState, makeMove) where

-- Original code from http://projects.haskell.org/operational/examples/TicTacToe.hs.html

import Data.Either
import Data.List

    -- external libraries needed
import System.Random


data Symbol = X | O deriving (Eq,Show)
type Square = Either Int Symbol
type Board = [[Square]]
data GameState = Game { board :: Board, activePlayer :: Symbol } deriving (Show, Eq)

initialGameState :: GameState
initialGameState = Game (map (map Left) [[1,2,3],[4,5,6],[7,8,9]]) X

    -- list the possible moves to play
possibleMoves :: Board -> [Int]
possibleMoves board = [k | Left k <- concat board]

    -- play a stone at a square
makeMove :: Int -> GameState -> Maybe GameState
makeMove k (Game board player)
    | not (k `elem` possibleMoves board) = Nothing   -- illegal move
    | otherwise = Just $ Game (map (map replace) board) (switch player)
    where
    replace (Left k') | k' == k = Right player
    replace x                   = x

    switch X = O
    switch O = X

    -- has somebody won the game?
won :: GameState -> Bool
won (Game board _) = any full $ diagonals board ++ rows board ++ cols board
    where
    full [a,b,c] = a == b && b == c
    diagonals [[a1,_,b1],
               [_ ,c,_ ],
               [b2,_,a2]] = [[a1,c,a2],[b1,c,b2]]
    rows = id
    cols = transpose

    -- is the game a draw?
draw :: GameState -> Bool
draw (Game board _) = null (possibleMoves board)

    -- print the board
showSquare = either (\n -> " " ++ show n ++ " ") (concat . replicate 3 . show)

showBoard :: Board -> String
showBoard board =
      unlines . surround "+---+---+---+"
    . map (concat . surround "|". map showSquare)
    $ board
    where
    surround x xs = [x] ++ intersperse x xs ++ [x]

printBoard = putStr . showBoard

