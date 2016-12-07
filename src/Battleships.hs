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
addShipRandom :: Board -> ShipType -> Board
addShipRandom = undefined


-- adds a ship at the selected position
isShipAddOk :: Board -> Ship -> Position -> Bool
isShipAddOk (Board matrix) (Ship ori shipT) (Position x y) | ori == Horizontal =
               isShipAddOk' x 0 (shipSize shipT) (matrix !! y)


                                                           | ori == Vertical = undefined
       where
          isShipAddOk' :: Int -> Int -> Int -> [Block] -> Bool
          isShipAddOk' x i sSize list | i < sSize = ((list !! (x+i)) == Unknown) && isShipAddOk' x (i+1) sSize list
                                      | otherwise = True



addShip :: Board -> Ship -> Position -> Board
addShip board (Ship orientation shiptype) pos = undefined


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
nbrOfHitsLeft (Board []) = 0
nbrOfHitsLeft (Board (x:xs)) = nbrOfHitsLeft' x + nbrOfHitsLeft (Board xs)
    where nbrOfHitsLeft' :: [Block] -> Int
          nbrOfHitsLeft' [] = 0
          nbrOfHitsLeft' (ShipPart:xs) = 1 + nbrOfHitsLeft' xs
          nbrOfHitsLeft' (_:xs)   = nbrOfHitsLeft' xs

-- prints a game
printGame :: Game -> IO ()
printGame (Game (Board b1) (Board b2)) = putStrLn("-----Your ships----\n"
    ++ printGame' False b1 ++ "\n----Enemy ships----\n" ++ printGame' True b2)
    where printGame' :: Bool -> [[Block]] -> String
          printGame' secret b = concatMap (++"\n") (map (concatMap (printBlock secret)) b)
          printBlock :: Bool -> Block -> String
          printBlock _ Hit = "x "
          printBlock _ Miss = "0 "
          printBlock _ Unknown = "~ "
          printBlock _ Swell = "~ "
          printBlock True ShipPart = "~ "
          printBlock _ ShipPart = "• "


readGame :: FilePath -> IO Game
readGame = undefined

-- shoots at selected position. If there is a ship at the position it will get
-- hit.
shoot :: Board -> Position -> Board
shoot (Board b) (Position x y) = Board (take y b ++ [shoot' (b !! y) x] ++ drop (y+1) b)
    where shoot' :: [Block] -> Int -> [Block]
          shoot' b x = take x b ++ [shoot'' (b !! x)] ++ drop (x+1) b
          shoot'' :: Block -> Block
          shoot'' ShipPart = Hit
          shoot'' Hit      = Hit
          shoot'' b        = Miss



gameOver :: Board -> Bool
gameOver = undefined
