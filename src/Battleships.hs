module Battleship where

import DataTypes
import RunGame
import Data.List

impl = Interface
   { iNewGame = newGame
   , iPrintGame = printGame
   , iWinnerIs = winnerIs
   , iGameOver = gameOver
   , iShoot = shoot
   }

main :: IO ()
main = runGame impl

-- new game
newGame :: Game
newGame = Game (newGame' emptyBoard)  (newGame' emptyBoard) --TODO: should be fillBoard
    where newGame' :: Board -> Board -- for debug only
          newGame' b = setBlock b (Position 5 5) ShipPart


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
-}{-
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
-}

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
nbrOfHits b = nbrOf b Hit

-- returns the minimum number of hits required to win
nbrOfHitsLeft :: Board -> Int
nbrOfHitsLeft b = nbrOf b ShipPart

-- counts the number of elements of a block type in a board
nbrOf :: Board -> Block -> Int
nbrOf (Board []) block     = 0
nbrOf (Board (x:xs)) block = nbrOf' x block + nbrOf (Board xs) block
    where nbrOf' :: [Block] -> Block -> Int
          nbrOf' xs block = length (filter (\x -> x == block) xs)

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

-- shoots on a position
shoot :: Board -> Position -> Board
shoot b pos = setBlock b pos (shoot'' (getBlock b pos))
    where shoot'' :: Block -> Block
          shoot'' ShipPart = Hit
          shoot'' Hit      = Hit
          shoot'' b        = Miss

-- checks if shot hit ship
isHit :: Board -> Position -> Bool
isHit b p | getBlock b p == Hit = True
          | otherwise = False

-- player computer shoots
computerShoot :: Board -> Board
computerShoot = undefined

-- returns a random posisiton unexplored
getRandomPositionUnexplored :: Board -> Position
getRandomPositionUnexplored = undefined

-- returns a neighbour that could be a part of a Ship
getPossibleNeighbourShip :: Board -> Position -> Position
getPossibleNeighbourShip = undefined

-- lists positions of blocks that for the player is unexplored (neither of type
-- hit nor miss)
listUnexplored :: Board -> [Position]
listUnexplored = undefined

-- list positions of blocks that are Hit
listHits :: Board -> [Position]
listHits (Board b) = listHits' 0 b
    where listHits' :: Int -> [[Block]] -> [Position]
          listHits' _ [] = []
          listHits' y (x:xs) = listHits'' 0 y x ++ listHits' (y+1) xs
          listHits'' :: Int -> Int -> [Block] -> [Position]
          listHits'' _ _ [] = []
          listHits'' x y (Hit:xs) = [Position x y] ++ listHits'' (x+1) y xs
          listHits'' x y (_:xs) = listHits'' (x+1) y xs

-- lists all neighouring blocks of position as a list of tuples (Position,Block)
listAllNeighbours :: Board -> Position -> [(Position, Block)]
listAllNeighbours b (Position x y) = getBlockIfValid b (Position (x+1) (y+1))
    ++ getBlockIfValid b (Position (x-1) (y-1))
    ++ getBlockIfValid b (Position (x-1) (y+1))
    ++ getBlockIfValid b (Position (x+1) (y-1))
    ++ listNeighbours b (Position x y)

-- list "direct" neighbouring blocks of position as a list of tuples (Position,Block)
listNeighbours :: Board -> Position -> [(Position, Block)]
listNeighbours b (Position x y) = getBlockIfValid b (Position (x+1) y)
    ++ getBlockIfValid b (Position (x-1) y)
    ++ getBlockIfValid b (Position x (y+1))
    ++ getBlockIfValid b (Position x (y-1))

-- returns position and block if valid block
getBlockIfValid :: Board -> Position -> [(Position,Block)]
getBlockIfValid b p | isValid p = [(p,getBlock b p)]
                    | otherwise = []

-- checks if position is within game board boundaries
isValid :: Position -> Bool
isValid (Position x y) | x>=0 && x<=9 && y>=0 && y<=9 = True
                       | otherwise = False

-- sets a block to a block type
setBlock :: Board -> Position -> Block -> Board
setBlock (Board b) (Position x y) block = Board (take y b ++ [setBlock' (b !! y) x block] ++ drop (y+1) b)
    where setBlock' :: [Block] -> Int -> Block -> [Block]
          setBlock' b x block = take x b ++ [block] ++ drop (x+1) b

-- returns the block at given position
getBlock :: Board -> Position -> Block
getBlock (Board b) (Position x y) = (b !! y) !! x

-- checks if game is over for any of the players
gameOver :: Game -> Bool
gameOver (Game b1 b2) = boardComplete b1 || boardComplete b2

-- checks if board is complete e.g. no ships left
boardComplete :: Board -> Bool
boardComplete b = nbrOfHitsLeft b == 0

-- returns the winner of the game TODO: error if no player won?
winnerIs :: Game -> Player
winnerIs (Game board1 board2) | boardComplete board1 = Player1
                              | otherwise = Player1
