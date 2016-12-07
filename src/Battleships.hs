module Battleship where

import DataTypes
import RunGame

-- tomt spelbräde
emptyBoard :: Board
emptyBoard = undefined


-- adds all ships to board
fillBoard :: Board -> Board
fillBoard = undefined


-- adds a ship at a random position of the board
addShipRandom :: Board -> Ship -> Board
addShipRandom = undefined


-- adds a ship at the selected position
addShip :: Board -> Ship -> Position -> Board
addShip = undefined


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
