module RunGame where

import Data.Char
import DataTypes
import System.Random

data Interface = Interface
    { iNewGame   :: StdGen -> Game
    , iPrintGame :: Game -> IO ()
    , iWinnerIs :: Game -> Player
    , iGameOver :: Game -> Bool
    , iShoot :: Board -> Position -> Board
    , iComputerShoot :: StdGen -> Board -> Board
    }

-- starts game
runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Battleships"
  g <- newStdGen
  gameLoop i (iNewGame i g) Player

-- loops game until someone wins
gameLoop :: Interface -> Game -> Player -> IO ()
gameLoop i (Game b1 b2) Computer = do
  if iGameOver i (Game b1 b2) then do
      quitGame i (Game b1 b2)
    else do
        g <- newStdGen
        let updBoard1 = iComputerShoot i g b1
        gameLoop i (Game updBoard1 b2) Player
gameLoop i (Game b1 b2) Player = do
  iPrintGame i (Game b1 b2)
  if iGameOver i (Game b1 b2) then do
      quitGame i (Game b1 b2)
    else do
        pos <- validPosition
        let updBoard2 = iShoot i b2 pos
        gameLoop i (Game b1 updBoard2) Computer

-- asks player for coordinates to shoot, returns position
validPosition :: IO Position
validPosition = do
   putStrLn "choose position to shoot [x y]"
   answer <- getLine
   if (length answer == 3) && isDigit (head answer)
     && isDigit (head (drop 2 answer))
     then return (getPosition answer)
     else do
       putStrLn "didn't quite catch that..."
       h <- validPosition
       return h
   where getPosition :: String -> Position
         getPosition answer = Position (digitToInt (head answer)) (digitToInt (head(drop 2 answer)))

-- presents the winner of the game
quitGame :: Interface -> Game -> IO ()
quitGame i (Game b1 b2) = putStrLn ("Game over! " ++ show (iWinnerIs i (Game b1 b2)) ++ " won!")
