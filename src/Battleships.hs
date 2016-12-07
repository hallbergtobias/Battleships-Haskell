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


printGame :: Game -> IO ()
printGame = undefined


readGame :: FilePath -> IO Game
readGame = undefined


shoot :: Board -> Position -> Board
shoot = undefined



gameOver :: Board -> Bool
gameOver = undefined
