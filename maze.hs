-- Author: Carson Graham
-- This code is licensed under GNU General Public License v3.0
-- 
--
-- Notes:
--
-- There's a saying in haskell - "if it compiles, it'll run perfectly"
-- because it's immutable/functional with a superb type system,
-- code is generally written correctly the first time
-- 
-- and testing is extreacmly easy, because everything is seperate from everything else
--
-- there are no mutating values that are changing incorrectly
-- there are no function calls with side affects
-- there are no functions you can't call
--
-- all and all, it's very nice. I like it
--
-- this code probably had 10x less the runtime-level bugs then an identical program written in Java

import Debug.Trace
import Data.List
import Data.Maybe
import Control.Monad

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replace (n-1) newVal xs

data Square = Wall | Empty | Travelled | Start | End
  deriving (Eq)

instance Show Square where
  show x = case x of
               Wall -> "W"
               Empty -> " "
               Travelled -> "*"
               Start -> "S"
               End -> "E"

data Maze = Maze [[Square]]

instance Show Maze where
  show (Maze f) = foldl (\all line -> all ++ "\n" ++ 
    (foldl (\all2 char -> all2 ++ (show char) ++ " ") "" line)) "" f

data Point = Point Int Int
  deriving (Eq)

instance Show Point where
  show (Point x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"


surrounding :: Point -> [Point]
surrounding (Point x y) = map (\ls -> Point (x + (ls!!0)) (y + (ls!!1))) [[0,1],[0,-1],[1,0],[-1,0]] 

inBounds :: Maze -> Point -> Bool
inBounds (Maze ls) (Point x y) = x >= 0 && 
                                 x < (length ls) &&
                                 y >= 0 &&
                                 y < (length (ls !! x))
get :: Maze -> Point -> Square
get (Maze ls) (Point x y) = (ls !! x) !! y

set :: Maze -> Point -> Square -> Maze
set (Maze ls) (Point x y) newSquare = Maze $ replace x (replace y newSquare (ls!!x)) ls

setTravelled :: Maze -> Point -> Maze
setTravelled maze point = do
  if get maze point == Empty then (set maze point Travelled) else maze

solveMazeFrom :: Maze -> Point -> [Maze]
solveMazeFrom maze startPoint = do
  if get maze startPoint == End then [maze] else do
    let possible  = filter (inBounds maze) (surrounding startPoint)
    let possible2 = filter (\newPoint -> (get maze newPoint == Empty) ||(get maze newPoint == End)) possible 
    if null possible2 then [] else do concat $ map (\newPoint -> solveMazeFrom (setTravelled maze startPoint) newPoint) possible2
      
findStarting :: Maze -> Point
findStarting (Maze ls) = do
  let row = fst $ head $ filter (\(index,line) -> Start `elem` line ) $ zip [0..] ls
  let col = fromJust $ elemIndex Start (ls !! row)
  (Point row col) 

solve :: Maze -> [Maze]
solve maze = do
  let start = findStarting maze
  solveMazeFrom maze start

-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
splitWhen     :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'

charToSquare :: Char -> Square
charToSquare 'W' = Wall
charToSquare '.' = Empty 
charToSquare 'S' = Start 
charToSquare 'E' = End


readMaze :: String -> IO Maze
readMaze filename = do
  lines <- splitWhen (=='\n') <$> readFile filename
  return $ Maze $ map (\line -> map charToSquare line) lines


solveMazeIO :: String -> IO [Maze]
solveMazeIO filename = solve <$> readMaze filename

main_ :: Int -> IO [Maze]
main_ i = solveMazeIO $ "maze" ++ show i ++ ".txt"



-- [
--
--
--
--
-- here is some sample output and testing of the program
--
--
-- *Main> :l maze.hs
-- [1 of 1] Compiling Main             ( maze.hs, interpreted )
-- Ok, one module loaded.

-- *Main> readMaze "maze1.txt"
-- W W W W W W W W
-- W         W   W
-- W W   W W     W
-- W         W   W
-- W   W   W W   E
-- S   W   W W   W
-- W W           W
-- W W W W W W W W

-- *Main> solveMazeIO "maze1.txt"
-- [
-- W W W W W W W W
-- W         W   W
-- W W   W W     W
-- W * * *   W   W
-- W * W * W W * E
-- S * W * W W * W
-- W W   * * * * W
-- W W W W W W W W ]

-- *Main> main_ 2
-- [
-- W W W S W W W W W W
-- W     * * W   W   W
-- W W W W * *       W
-- W * * * W * W W   W
-- W * W * * *   W   W
-- W E W W W W W W W W ]

-- *Main> main_ 3
-- [
--    W W
-- W * * S
-- E * W W ]

-- *Main> take 10 $ map (\x -> 2^x) [0..]
-- [1,2,4,8,16,32,64,128,256,512]


