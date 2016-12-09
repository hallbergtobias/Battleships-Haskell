module Battleship where

import DataTypes
import RunGame

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

-- player computer shoots
computerShoot :: Board -> Board
computerShoot = undefined

-- lists positions of blocks that for the player is unexplored (neither of type
-- hit nor miss)
listUnexplored :: Board -> [Position]
listUnexplored = undefined


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
