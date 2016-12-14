module Battleship where

import DataTypes
import RunGame
import Data.List
import System.Random

impl = Interface
   { iNewGame = newGame
   , iPrintGame = printGame
   , iWinnerIs = winnerIs
   , iGameOver = gameOver
   , iShoot = shoot
   , iComputerShoot = computerShoot
   }

main :: IO ()
main = runGame impl

level1 = Level [(Carrier,1),(Battleship,2),(Cruiser,3),(Submarine,4),(Destroyer,5)]
level2 = Level [(Carrier,1),(Battleship,2)]

-- new game
newGame :: StdGen -> Game
newGame g = createGame g level2

-- creates a game where ships according to Level have been randomly positioned
createGame :: StdGen -> Level -> Game
createGame g lvl = Game (createBoard g lvl emptyBoard) (createBoard g lvl emptyBoard)
    where createBoard :: StdGen -> Level -> Board -> Board
          createBoard _ (Level []) b = b
          createBoard g (Level ((s,n):xs)) b = createBoard g (Level xs) (createBoard' g s n b)
              where createBoard' :: StdGen -> ShipType -> Int -> Board -> Board
                    createBoard' _ _ 0 b = b
                    createBoard' g s n b = createBoard' g1 s (n-1) (addShipRandom g1 b s)
                        where (r,g1) = random g :: (Int,StdGen)

-- Returns an empty board
emptyBoard :: Board
emptyBoard = Board (replicate 10 (replicate 10 Water))

-- Adds a ship at a random position on the board
addShipRandom :: StdGen -> Board -> ShipType -> Board
addShipRandom g b s = addShipRandom' g b s (getRandomOrientation g)
  where addShipRandom' :: StdGen -> Board -> ShipType -> Orientation -> Board
        addShipRandom' g b s o | isShipAddOk b ship pos = addShip b ship pos
                               | otherwise = addShipRandom' g3 b s o
            where ship = Ship o s
                  pos = (Position x y)
                  (x,g2) = randomR (0, maxX ship) g
                  (y,g3) = randomR (1, maxY ship) g2
                  -- returns maximum possible x-coordinate to add ship at
                  maxX :: Ship -> Int
                  maxX (Ship Vertical _) = 9
                  maxX (Ship Horizontal s) = 10 - (shipSize s)
                  -- returns maximum possible y-coordinate to add ship at
                  maxY :: Ship -> Int
                  maxY (Ship Vertical s) = 10 - (shipSize s)
                  maxY (Ship Horizontal _) = 9

-- returns a random orientation, Horizontal or Vertical
getRandomOrientation :: StdGen -> Orientation
getRandomOrientation g | (getRandom g)==0 = Horizontal
    where getRandom :: StdGen -> Int
          getRandom g = o
              where (o,g2) = randomR (0, 1) g
getRandomOrientation g | otherwise = Vertical


-- Checks whether the given position is a valid position to place a ship in.
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


-- Checks whether or not isShipAddOk is working by making sure that isShipAddOk
-- gives the same results as prop_addShip.
prop_isShipAddOk :: Board -> Ship -> Position -> Bool
prop_isShipAddOk board ship pos = ((isShipAddOk board ship pos) == prop_addShip board ship pos)


-- Blindly adds a ship to the board with the upper left of the ship being at the given
-- starting position.
addShip :: Board -> Ship -> Position -> Board
addShip board (Ship Horizontal shipType) (Position x y) = addHor board y (map (+x) [0..((shipSize shipType)-1)])
    where
      addHor :: Board -> Int -> [Int] -> Board
      addHor board y [x] = setBlock board (Position x y) ShipPart
      addHor (Board matrix) y (x:xs) = addHor (setBlock (Board matrix) (Position x y) ShipPart) y xs

addShip board (Ship Vertical shipType) (Position x y) = addVer board x (map (+y) [0..((shipSize shipType)-1)])
    where
      addVer :: Board -> Int -> [Int] -> Board
      addVer board x [y] = setBlock board (Position x y) ShipPart
      addVer (Board matrix) x (y:ys) = addVer (setBlock (Board matrix) (Position x y) ShipPart) x ys


addSwell :: Board -> [Position] -> Board
addSwell board [x] | isValid x = setBlock board x Swell
addSwell board [x] | otherwise = board
addSwell board (x:xs) | isValid x = addSwell (setBlock board x Swell) xs
addSwell board (x:xs) | otherwise = addSwell board xs





getSwellPositions :: [Position] -> Orientation -> [Position]
getSwellPositions pos o = getSides pos o ++ getCorners pos o
    where -- returns positions of Swell on side of ship
          getSides :: [Position] -> Orientation -> [Position]
          getSides pos Horizontal = moveShip pos 0 (-1) ++ moveShip pos 0 1
          getSides pos Vertical = moveShip pos (-1) 0 ++ moveShip pos 1 0
          moveShip :: [Position] -> Int -> Int -> [Position]
          moveShip [] _ _ = []
          moveShip ((Position x y):as) dx dy = Position (x+dx) (y+dy) : moveShip as dx dy
          -- returns positions of Swell in "corner" of ship
          getCorners :: [Position] -> Orientation -> [Position]
          getCorners pos Horizontal = getCorners' (head pos) Horizontal (-1) 0 ++ getCorners' (last pos) Horizontal 1 0
          getCorners pos Vertical = getCorners' (head pos) Vertical 0 (-1) ++ getCorners' (last pos) Vertical 0 1
          getCorners' :: Position -> Orientation -> Int -> Int -> [Position]
          getCorners' (Position x y) o dx dy = makeThree (Position (x+dx) (y+dy)) o
              where makeThree :: Position -> Orientation -> [Position]
                    makeThree (Position x y) Vertical = [Position (x-1) y] ++ [Position x y] ++ [Position (x+1) y]
                    makeThree (Position x y) Horizontal = [Position x (y-1)] ++ [Position x y] ++ [Position x (y+1)]


-- Tests if addShip really adds a ship at the given positon by first counting
-- the number of ShipParts on the board before and after adding to make sure that
-- the correct number of ShipParts were added and then checking so that there is
-- a ShipPart at every position of the added ship.
--
-- Because of addShip not checking if the position is valid to add a ship at,
-- make sure to send in valid positions in this propertycheck by using isShipAddOk first.
prop_addShip :: Board -> Ship -> Position -> Bool
prop_addShip board (Ship ori shipType) pos = (((nbrOf board ShipPart) ==
  ((nbrOf (addShip board (Ship ori shipType) pos) ShipPart) - shipSize shipType))
  && prop_addShip' board (Ship ori shipType) pos)
  where
     prop_addShip' :: Board -> Ship -> Position -> Bool
     prop_addShip' board (Ship Horizontal shipType) (Position x y) =
       prop_addShipHor (addShip board (Ship Horizontal shipType) (Position x y)) y (map (+x) [0..((shipSize shipType)-1)])
              where
                prop_addShipHor :: Board -> Int -> [Int] -> Bool
                prop_addShipHor _ _ [] = True
                prop_addShipHor (Board matrix) y (x:xs) =
                  (((matrix !! y) !! x) == ShipPart) && (prop_addShipHor (Board matrix) y xs)
     prop_addShip' board (Ship Vertical shipType) (Position x y) =
       prop_addShipVer (addShip board (Ship Vertical shipType) (Position x y)) x (map (+y) [0..((shipSize shipType)-1)])
              where
                prop_addShipVer :: Board -> Int -> [Int] -> Bool
                prop_addShipVer _ _ [] = True
                prop_addShipVer (Board matrix) x (y:ys) =
                  (((matrix !! x) !! y) == ShipPart) && (prop_addShipVer (Board matrix) x ys)



-- returns the size of a ship
shipSize :: ShipType -> Int
shipSize Destroyer = 1
shipSize Submarine = 2
shipSize Cruiser = 3
shipSize Battleship = 4
shipSize Carrier = 5

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
    ++ printGame' Player b1 ++ "\n----Enemy ships----\n" ++ printGame' Computer b2)
    where printGame' :: Player -> [[Block]] -> String
          printGame' player b = unlines (map (concatMap (printBlock player)) b)
          printBlock :: Player -> Block -> String
          printBlock _ Hit = "x "
          printBlock _ Miss = "0 "
          printBlock _ Water = "~ "
          printBlock _ Swell = "~ "
          printBlock Computer ShipPart = "~ "
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
