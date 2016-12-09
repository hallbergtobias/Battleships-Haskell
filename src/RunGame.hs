module RunGame where

import Data.Char
import DataTypes

data Interface = Interface
    { iNewGame   :: Game
    , iPrintGame :: Game -> IO ()
    , iWinnerIs :: Game -> Player
    , iGameOver :: Game -> Bool
    , iShoot :: Board -> Position -> Board
    }

runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Welcome"
  gameLoop i (iNewGame i)

gameLoop :: Interface -> Game -> IO ()
gameLoop i (Game b1 b2) = do
  iPrintGame i (Game b1 b2)
  if iGameOver i (Game b1 b2) then do
      undefined
    else do
      putStrLn "choose position to shoot [x y]"
      answer <- getLine
      let x = digitToInt (head answer)
          y = digitToInt (last answer)
          updb2 = iShoot i b2 (Position x y)
      gameLoop i (Game b1 updb2)
