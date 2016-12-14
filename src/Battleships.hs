module Battleship where

import DataTypes
import RunGame
import Data.List
import System.Random

impl = Interface
   { iNewGame = newGame
   , iTestGame = testGame
   , iPrintGame = printGame
   , iWinnerIs = winnerIs
   , iGameOver = gameOver
   , iShoot = shoot
   , iComputerShoot = computerShoot
   }

main :: IO ()
main = runGame impl

-- new game
newGame :: Game
newGame = Game (newGame' emptyBoard)  (newGame' emptyBoard) --TODO: should be fillBoard
    where newGame' :: Board -> Board -- for debug only
          newGame' b = setBlock b (Position 5 5) ShipPart

testGame :: Game
testGame = Game (Board [[Water,Water,Water,Water,Water,Water,Water,Water,Water,Water],[Water,Water,Water,Water,Water,Water,Water,Water,Water,Water],[Water,Water,Water,Water,Water,Water,Water,Water,Water,Water],[Water,Water,Water,Water,Water,Water,Water,Water,Water,Water],[Water,Water,Water,Water,Water,Water,Water,Water,Water,Water],[Water,Water,Water,Water,ShipPart,Water,Water,Water,Water,Water],[Water,Water,Water,Water,ShipPart,Water,Water,Water,Water,Water],[Water,Water,Water,Water,ShipPart,Water,Water,Water,Water,Water],[Water,Water,Water,Water,ShipPart,Water,Water,Water,Water,Water],[Water,Water,Water,Water,ShipPart,Water,Water,Water,Water,Water]]) (Board [[Water,Water,Water,Water,Water,Water,Water,Water,Water,Water],[Water,Water,Water,Water,Water,Water,Water,Water,Water,Water],[Water,Miss,ShipPart,ShipPart,ShipPart,ShipPart,Water,Water,Water,Water],[Water,Water,Water,Water,Water,Water,Water,Water,Water,Water],[Water,Water,Water,Water,Water,Water,Water,Water,Water,Water],[Water,Water,Water,Water,ShipPart,Water,Water,Water,Water,Water],[Water,Water,Water,Water,ShipPart,Water,Water,Water,Water,Water],[Water,Water,Water,Water,ShipPart,Water,Water,Water,Water,Water],[Water,Water,Water,Water,ShipPart,Water,Water,Water,Water,Water],[Water,Water,Water,Water,ShipPart,Water,Water,Water,Water,Water]])

-- Returns an empty board
emptyBoard :: Board
emptyBoard = Board (replicate 10 (replicate 10 Water))


-- Adds all ships to board
fillBoard :: Board -> Board
fillBoard = undefined


-- Adds a ship at a random position on the board
addShipRandom :: Board -> ShipType -> Board
addShipRandom = undefined


-- CHANGE TO NO GUARDS
-- Checks if the given position is a valid place for a ship.
isShipAddOk :: Board -> Ship -> Position -> Bool
isShipAddOk (Board matrix) (Ship ori shipT) (Position x y) | ori == Horizontal =
               isShipAddOk' x 0 (shipSize shipT) (matrix !! y)

                                                           | ori == Vertical =
               isShipAddOk' y 0 (shipSize shipT) (vertList (map (drop x) matrix))
       where
          isShipAddOk' :: Int -> Int -> Int -> [Block] -> Bool
          isShipAddOk' x i sSize list | i < sSize = ((list !! (x+i)) == Water) && isShipAddOk' x (i+1) sSize list
                                      | otherwise = True
          vertList :: [[Block]] -> [Block]
          vertList [] = []
          vertList ((x:xs):ys) = [x] ++ vertList ys

{-}
-- Adds a ship to the board with the upper left of the ship being at the given
-- starting position. If the position is unvalid or there already lies a ship or
-- ship swell there it returns an error.
addShip :: Board -> Ship -> Position -> Board
addShip board ship (Position x y) | x < 0 || x > 9 || y < 0 || y > 9 = error "That position is out of bounds"
                                  | isShipAddOk board ship (Position x y) = addShip' board ship (Position x y)
                                  | otherwise = error "There is already a ship there, use eyes maybe?"
          where
            addShip' :: Board -> Ship -> Position -> Board
            addShip' board (Ship Horizontal shiptype) pos =
              addShipHor board pos ((shipSize shiptype)-1)

            addShip' board (Ship Vertical shiptype) pos =
              addShipVert board pos ((shipSize shiptype)-1)

            addShipHor :: Board -> Position -> Int -> Board
            addShipHor board pos 0 = setBlock board pos ShipPart
            addShipHor (Board matrix) (Position x y) i =
              addShipHor (setBlock (Board matrix) (Position (x+i) y) ShipPart) (Position x y) (i-1)

            addShipVert :: Board -> Position -> Int -> Board
            addShipVert board pos 0 = setBlock board pos ShipPart
            addShipVert (Board matrix) (Position x y) i =
              addShipVert (setBlock (Board matrix) (Position x (y+i)) ShipPart) (Position x y) (i-1)
-}

-- Adds a ship to the board with the upper left of the ship being at the given
-- starting position. If the position is unvalid or there already lies a ship or
-- ship swell there it returns an error.
addShip :: Board -> Ship -> Position -> Board
addShip board ship (Position x y) | x < 0 || x > 9 || y < 0 || y > 9 = error "That position is out of bounds"
                       | isShipAddOk board ship (Position x y) = addShip' board ship (Position x y)
                       | otherwise = error "Unvalid position: The position is touching another ship's or its surrounding blocks's"

addShip' :: Board -> Ship -> Position -> Board
addShip' board (Ship Horizontal shipType) (Position x y)
  = addShipHor board y (map (+x) [0..((shipSize shipType)-1)])
    where
      addShipHor :: Board -> Int -> [Int] -> Board
      addShipHor board y [x] = setBlock board (Position x y) ShipPart
      addShipHor (Board matrix) y (x:xs) = addShipHor (setBlock (Board matrix) (Position x y) ShipPart) y xs
addShip' board (Ship Vertical shipType) (Position x y)
  = addShipVer board x (map (+y) [0..((shipSize shipType)-1)])
    where
      addShipVer :: Board -> Int -> [Int] -> Board
      addShipVer board x [y] = setBlock board (Position x y) ShipPart
      addShipVer (Board matrix) x (y:ys) =  addShipVer (setBlock (Board matrix) (Position x y) ShipPart) x ys


{-
-- Tests if addShip really adds a ship at the given positon.
prop_addShip :: Board -> Ship -> Position -> Bool
prop_addShip board (Ship Horizontal shipType) pos =
                           prop_addShipHor (addShip board (shipSize shipType) pos)
prop_addShip board (Ship Vertical shipType) pos =
                           prop_addShipVert (addShip board (shipSize shipType) pos)
                           where
                              prop_addShipHor :: Board -> Int -> Position -> Bool
                              prop_addShipHor _ 0 _ =
                              prop_addShipHor board i (Position x y) =


board = emptyBoard
shipH = (Ship Horizontal Carrier)
shipV = (Ship Vertical Battleship)
pos1 = (Position 1 6)
pos2 = (Position 2 3)


-}

-- Tests if addShip really adds a ship at the given positon by using addShip
-- and then checking if there is a ShipPart at every position of the added ship.
prop_addShip :: Board -> Ship -> Position -> Bool
prop_addShip board (Ship Horizontal shipType) (Position x y) =
  prop_addShipHor (addShip board (Ship Horizontal shipType) (Position x y)) y (map (+x) [0..((shipSize shipType)-1)])
              where
                prop_addShipHor :: Board -> Int -> [Int] -> Bool
                prop_addShipHor _ _ [] = True
                prop_addShipHor (Board matrix) y (x:xs) =
                  (((matrix !! y) !! x) == ShipPart) && (prop_addShipHor (Board matrix) y xs)
prop_addShip board (Ship Vertical shipType) (Position x y) =
  prop_addShipVer (addShip board (Ship Vertical shipType) (Position x y)) x (map (+y) [0..((shipSize shipType)-1)])
              where
                prop_addShipVer :: Board -> Int -> [Int] -> Bool
                prop_addShipVer _ _ [] = True
                prop_addShipVer (Board matrix) x (y:ys) =
                  (((matrix !! x) !! y) == ShipPart) && (prop_addShipVer (Board matrix) x ys)



{-
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
          printBlock _ Water = "~ "
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
computerShoot :: StdGen -> Board -> Board
computerShoot g b = shoot b (computerShoot' g b)
    where computerShoot' :: StdGen -> Board -> Position
          computerShoot' g b = comp'' g b hits
              where hits = listHits b
          comp'' :: StdGen -> Board -> [Position] -> Position
          comp'' g b [] = getRandomPositionUnexplored g b
          comp'' g b (x:xs) | null validPositions = comp'' g b xs
                            | otherwise = head validPositions
              where validPositions = filter isValid pos
                    pos = getPossibleNeighbourShip b x

-- returns a random posisiton unexplored
getRandomPositionUnexplored :: StdGen -> Board -> Position
getRandomPositionUnexplored stdgen board = list !! index
    where list = listUnexplored board
          (index,g2) = randomR (1, length list) stdgen

-- takes a block and a position that is a block of type Hit
-- returns a neighbour that could be a potential ShipPart
getPossibleNeighbourShip :: Board -> Position -> [Position]
getPossibleNeighbourShip b pos = getPossible b (listNeighbours b pos) pos
    where getPossible :: Board -> [(Position, Block)] -> Position -> [Position]
          getPossible b list pos = getPossible' b list pos (countBlock blocks Hit)
              where (p,blocks) = unzip list
          getPossible' :: Board -> [(Position, Block)] -> Position -> Int -> [Position]
          getPossible' _ list _ 0 = getUnknowns list
          getPossible' b list pos 1 | not (isBlockShotAt b opposite) = [opposite]
              where opposite = getOpposite pos (getPositionOfHit list)
          getPossible' _ _ _ _ = []
          -- takes two positions, x and y, returns opposite of y from x
          getOpposite :: Position -> Position -> Position
          getOpposite (Position a b) (Position c d) = Position (a+(a-c)) (b+(b-d))
          -- returns position of Hit
          getPositionOfHit :: [(Position, Block)] -> Position
          getPositionOfHit ((p,Hit):xs) = p
          getPositionOfHit (x:xs) = getPositionOfHit xs
          -- returns positions of unknown blocks
          getUnknowns :: [(Position, Block)] -> [Position]
          getUnknowns [] = []
          getUnknowns ((pos,Hit):xs) = getUnknowns xs
          getUnknowns ((pos,Miss):xs) = getUnknowns xs
          getUnknowns ((pos,block):xs) = [pos] ++ getUnknowns xs

-- returns True if block is shot at
isBlockShotAt :: Board -> Position -> Bool
isBlockShotAt b pos | block==Hit || block == Miss = True
                    | otherwise = False
    where block = getBlock b pos

-- counts occurenses of a block in list
countBlock :: [Block] -> Block -> Int
countBlock blocks b = length (filter (==b) blocks)

-- lists positions of blocks that for the player is unexplored (neither of type
-- hit nor miss)
listUnexplored :: Board -> [Position]
listUnexplored b = listPositionsOfBlock b Water
    ++ listPositionsOfBlock b ShipPart
    ++ listPositionsOfBlock b Swell

-- list positions of blocks that are Hit
listHits :: Board -> [Position]
listHits b = listPositionsOfBlock b Hit

listPositionsOfBlock :: Board -> Block -> [Position]
listPositionsOfBlock (Board board) block = listPos' 0 board block
    where listPos' :: Int -> [[Block]] -> Block -> [Position]
          listPos' _ [] b = []
          listPos' y (row:rows) b = listPos'' 0 y row b ++ listPos' (y+1) rows b
          listPos'' :: Int -> Int -> [Block] -> Block -> [Position]
          listPos'' _ _ [] b = []
          listPos'' x y (block:row) b | block==b = [Position x y] ++ listPos'' (x+1) y row b
                                      | otherwise = listPos'' (x+1) y row b

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
winnerIs (Game board1 board2) | boardComplete board1 = Computer
                              | otherwise = Player
