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

-- starts game
runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Battleships"
  gameLoop i (iNewGame i)

-- loops game until someone wins
gameLoop :: Interface -> Game -> IO ()
gameLoop i (Game b1 b2) = do
  iPrintGame i (Game b1 b2)
  if iGameOver i (Game b1 b2) then do
      putStrLn ("Game over! " ++ show (iWinnerIs i (Game b1 b2)) ++ " won!")
    else do
      answer <- validInput
      let (x,y) = getPosition answer
          updb2 = iShoot i b2 (Position x y)
      gameLoop i (Game b1 updb2)
      where getPosition :: String -> (Int,Int)
            getPosition answer = (digitToInt (head answer),digitToInt (head(drop 2 answer)))

-- returns input in the format "x y"
validInput :: IO String
validInput = do
   putStrLn "choose position to shoot [x y]"
   answer <- getLine
   if (length answer == 3) && isDigit (head answer)
     && isDigit (head (drop 2 answer))
     then return answer
     else do
       putStrLn "didn't quite catch that..."
       h <- validInput
       return h
