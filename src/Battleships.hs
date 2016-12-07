module Battleship where

import DataTypes
import RunGame

-- tomt spelbräde
emptyBoard :: Board
emptyBoard = Board (replicate 10 (replicate 10 Unknown))


-- adds all ships to board
fillBoard :: Board -> Board
fillBoard = undefined


-- adds a ship at a random position of the board
addShipRandom :: Board -> Ship -> Board
addShipRandom = undefined


-- adds a ship at the selected position
addShipIfOk :: Board -> Ship -> Position -> Board
addShipIfOk board ship pos = undefined

addShip :: Board -> Ship -> Position -> Board
addShip board ship pos = undefined


-- returns the size of a ship
shipSize :: Ship -> Int
shipSize = undefined


-- return the number of ships on a board
nbrOfShips :: Board -> Int
nbrOfShips = undefined


-- returns the number of hits on a board
nbrOfHits :: Board -> Int
nbrOfHits = undefined


-- returns the minimum number of hits required to win
nbrOfHitsLeft :: Board -> Int
nbrOfHitsLeft = undefined


printGame :: Game -> IO ()
printGame = undefined


readGame :: FilePath -> IO Game
readGame = undefined


shoot :: Board -> Position -> Board
shoot = undefined


gameOver :: Board -> Bool
gameOver = undefined
