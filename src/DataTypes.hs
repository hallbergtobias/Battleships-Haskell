module DataTypes where

-- Game represents the game by linking two boards together, one for
-- the enemy and one for yourself.
data Game = Game {board1 :: Board, board2 :: Board}

-- A grid containting 10x10 blocks.
data Board = Board { rows :: [[Block]]}
    deriving (Show)

-- The blocks of the board are of the following states.
data Block = Water | ShipPart | Swell | Hit | Miss
    deriving (Eq,Show)

data Ship = Ship {orientation :: Orientation, shipType :: ShipType}

-- There are 5 different ship types which differ in size, from size 1
-- to size 5.
data ShipType = Destroyer | Submarine | Cruiser | Battleship | Carrier
    deriving (Show)

-- The ships either lie vertically or horizontally.
data Orientation = Vertical | Horizontal
    deriving (Eq,Show)

-- Represents a position on the board with x and y coordinates.
data Position = Position {x :: Int, y :: Int}
    deriving (Show)

data Player = Player | Computer
    deriving (Show)

data Level = Level {allShips :: [(ShipType,Int)]}
    deriving (Show)
