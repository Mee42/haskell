import Debug.Trace
import Data.List
import Data.Maybe

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

solveMazeFrom :: Maze -> Point -> Point -> [Maze]
solveMazeFrom maze startPoint fromPoint = do
  if get maze startPoint == End then trace "found end" [maze] else do
    let possible  = filter (\x -> x /= fromPoint) (surrounding startPoint)
    let possible2 = filter (\newPoint -> (get maze newPoint == Empty) ||(get maze newPoint == End)) possible 
    if null possible2 then [] else do concat $ map (\newPoint -> solveMazeFrom (set maze startPoint Travelled) newPoint startPoint) possible2
      
findStarting :: Maze -> (Point,Point)
findStarting (Maze ls) = do
  let row = fst $ head $ filter (\(index,line) -> Start `elem` line ) $ zip [0..] ls
  let col = fromJust $ elemIndex Start (ls !! row)
  ((Point row col),(Point row (col + 1))) 

solve :: Maze -> [Maze]
solve maze = do
  let (start,next) = findStarting maze
  solveMazeFrom maze next start

maze1 = Maze [
    replicate 8 Wall,
    Wall:(replicate 4 Empty) ++ [Wall,Empty,Wall],
    Wall : Wall : Empty : Wall : Wall : Empty : Empty : [Wall],
    Wall : Empty : Empty : Empty : Empty : Wall : Empty : Wall : [],
    Wall : Empty : Wall : Empty : Wall : Wall : Empty : End : [],
    Start : Empty : Wall : Empty : Wall : Wall : Empty : Wall : [],
    Wall : Wall : Empty : (take 4 (repeat Empty)) ++ [Wall],
    replicate 8 Wall]


maze2 = Maze [
    replicate 8 Wall,
    Wall : Empty : Empty : Empty : Empty : Wall : Empty : Wall : [],
    Wall : Wall : Empty : Wall : Wall : Empty : Empty : Wall : [],
    Wall : (replicate 4 Empty) ++ [Wall,Empty,Wall],
    Wall : Empty : Wall : Empty : Wall : Wall : Empty : [End],
    Start : Empty : Wall : Empty : Wall : Wall : Empty : Wall : [],
    Wall : Wall : (replicate 5 Empty) ++ [Wall],
    replicate 8 Wall]
    


















