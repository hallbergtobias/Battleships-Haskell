module Battleship where

import DataTypes
import RunGame
import Data.List

-- tomt spelbräde
emptyBoard :: Board
emptyBoard = Board (replicate 10 (replicate 10 Unknown))


-- adds all ships to board
fillBoard :: Board -> Board
fillBoard = undefined


-- adds a ship at a random position of the board
addShipRandom :: Board -> ShipType -> Board
addShipRandom = undefined


-- adds a ship at the selected position
-- CHANGE TO NO GUARDS
isShipAddOk :: Board -> Ship -> Position -> Bool
isShipAddOk (Board matrix) (Ship ori shipT) (Position x y) | ori == Horizontal =
               isShipAddOk' x 0 (shipSize shipT) (matrix !! y)

                                                           | ori == Vertical =
               isShipAddOk' y 0 (shipSize shipT) (vertList (map (drop x) matrix))
       where
          isShipAddOk' :: Int -> Int -> Int -> [Block] -> Bool
          isShipAddOk' x i sSize list | i < sSize = ((list !! (x+i)) == Unknown) && isShipAddOk' x (i+1) sSize list
                                      | otherwise = True
          vertList :: [[Block]] -> [Block]
          vertList [] = []
          vertList ((x:xs):ys) = [x] ++ vertList ys
{-
-- Sending Board all the way through?
addShip :: Board -> Ship -> Position -> Board
addShip board ship (Position x y) | x < 0 || x > 9 || y < 0 || y > 9 = error "That position is out of bounds"
                                  | isShipAddOk board ship (Position x y) = addShip'
                                  | otherwise = error "There is already a ship there, use eyes maybe?"
          where
            addShip' :: Board -> Ship -> Position -> Board
            addShip' board (Ship Horizontal shiptype) pos =
              addShip'' (Board matrix) pos ((shipSize shiptype)-1)

          --  addShip' (Board matrix) (Ship Vertical shiptype) (Position x y) =
            --  addShip''



            addShip'' :: Board -> Position -> Int -> Board
            addShip'' _ _ 0 = []
            addShip'' (Board matrix) (Position x y) i = setBlock (Position ((x+i) y) ShipPart matrix &&
                            addShip'' (Board matrix) (Position x y) (i-1)
-}
-- Takes a matrix of blocks and changes the block at the given position to the
-- specified block. Returns the resulting matrix.
setBlock :: Position -> Block -> [[Block]] -> [[Block]]
setBlock pos block matrix = setBlock' pos block 0 matrix
      where
         setBlock' :: Position -> Block -> Int -> [[Block]] -> [[Block]]
         setBlock' _ _ _ [] = []
         setBlock' (Position x y) block i (l:ls) | i < y = [l] ++ setBlock' (Position x y) block (i+1) ls
                                                      | otherwise = [setBlock'' (Position x y) block 0 l] ++ ls
         setBlock'' :: Position -> Block -> Int -> [Block] -> [Block]
         setBlock'' _ _ _ [] = []
         setBlock'' (Position x y) block j (l:ls) | j < x = [l] ++ (setBlock'' (Position x y) block (j+1) ls)
                                                       | otherwise = [block] ++ ls


-- returns the size of a ship
shipSize :: ShipType -> Int
shipSize Destroyer = 1
shipSize Submarine = 2
shipSize Cruiser = 3
shipSize Battleship = 4
shipSize Carrier = 5


-- return the number of ships on a board
nbrOfShips :: Board -> Int
nbrOfShips = undefined


-- returns the number of hits on a board
nbrOfHits :: Board -> Int
nbrOfHits (Board []) = 0
nbrOfHits (Board (x:xs)) = nbrOfHits' x + nbrOfHits (Board xs)
    where nbrOfHits' :: [Block] -> Int
          nbrOfHits' [] = 0
          nbrOfHits' (Hit:xs) = 1 + nbrOfHits' xs
          nbrOfHits' (_:xs)   = nbrOfHits' xs


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
