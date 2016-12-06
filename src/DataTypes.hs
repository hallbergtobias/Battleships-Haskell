
-- Game represents the game by linking two boards together, one for
-- the enemy and one for yourself.
data Game = Game {board1 :: Board, board2 :: Board}

-- A grid containting 10x10 blocks.
data Board = Board { rows :: [[Blocks]]}

-- The blocks of the board are of the following states.
data Block = Unknown | Vehichle | Swell | Hit | Miss

-- There are 5 different ship types which differ in size, from size 1
-- to size 5.
data Ship = Destroyer | Submarine | Cruiser | Battleship | Carrier

-- Represents a position on the board with x and y coordinates.
data Position = Position {x :: Int, y :: Int}
