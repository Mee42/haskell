import Text.Read
import Data.Maybe
import System.IO
import Data.List


data Player = X | O
    deriving (Eq,Show)

instance Enum Player where
    succ x = case x of
               X -> O
               O -> X
    toEnum x = if x `mod` 2 == 0 then X else O 
    fromEnum x = if x == X then 0 else 1


newtype Square = Square (Maybe Player)
    deriving (Eq)

instance Show Square where
    show (Square a) = case a of
      Just X  -> "X"
      Just O -> "O"
      Nothing -> "_"

data Board = Board [Square]

instance Show Board where
    show a = rowToString a 0 ++ line ++ (rowToString a 1) ++ line ++ (rowToString a 2)



rowToString :: Board -> Int -> String
rowToString (Board a) x = " " ++ (show (a !! (3*x + 0))) ++ " | " ++ (show (a !! (3*x+1))) ++ " | " ++ (show (a !! (3*x+2)))

line = "\n------------\n"

getPlayer :: Square -> Player
getPlayer (Square (Just a)) = a

winRow :: Board -> (Int,Int,Int) -> Maybe Player
winRow (Board a) (x,y,z) = if (a !! x) == (a !! y) && (a !! x) == (a !! z) && ((a !! x) /= Square Nothing)
                        then Just $ getPlayer (a !! x)
                        else Nothing

combine :: Maybe x -> Maybe x -> Maybe x
combine (Just a) (Just b)= Just a
combine (Just a) Nothing = Just a
combine Nothing (Just b) = Just b
combine Nothing Nothing  = Nothing

combineList :: [Maybe Player] -> Maybe Player
combineList list = foldl combine Nothing list

win :: Board -> Maybe GameWinner
win a = do
    let result = combineList $ map (winRow a) [(0,1,2),(3,4,5),(6,7,8),(0,3,6),(1,4,7),(2,5,8),(0,4,8),(2,4,6)]
    if isNothing result && isTie a
    then Just Tie
    else fmap (\x -> GameWinner x) result
 
isTie :: Board -> Bool
isTie (Board xs) = all (\x -> x == Square Nothing) xs

play :: Board -> Player -> Int -> Maybe Board
play (Board a) player index = 
  if index < 0 || index > 8 || (a !! index) /= Square Nothing
      then Nothing
  else Just $ Board $ take index a ++ [Square (Just player)] ++ drop (index + 1) a


emptyBoard = Board [ Square Nothing | x <- [0..8]]

data AIMode = PvP | PvC
    deriving Show


playerMove :: Board -> Player -> AIMode -> IO Board
playerMove board O PvC = return $ fromJust $ computerPlay board O
playerMove board player mode = do
                            putStrLn $ show board
                            putStrLn $ "Player " ++ show player ++ " turn"
                            move <- getIntInput "Move (0..8): " (\x -> x `elem` [0..8]) helpHelp
                            let new = play board player move
                            putStrLn $ maybe "invalid move" show new
                            if isJust new
                            then return $ fromJust new
                            else do
                              playerMove board player mode


updateElement :: Board -> Int -> Player -> Board
updateElement (Board a) index player = 
  Board $ take index a ++ [Square (Just player)] ++ drop (index + 1) a
computerPlay :: Board -> Player -> Maybe Board -- return the thing
computerPlay ex player = 
  maybe Nothing (\x -> Just (updateElement ex x player)) $ computerPlayX ex player

getList :: Board -> [Square]
getList (Board a) = a

computerPlayX :: Board -> Player -> Maybe Int
computerPlayX (Board ex) player = Nothing 
  

search :: Board -> Player -> [Fractional,Index]
search board player = do
  let list1 = zip (getList board) [x | x <- [0..8]]
  let options = filter (\(x,i) -> x == Square Nothing) list1
  let newBoards = map (\(x,i) -> ((updateElement board x player),i)) options 
  let scores = map(\(boardX,i) -> ((do {
    let win = win boardX;
    if isJust && fromJust win == Tie;
        then 0.1; -- tie
    else if isJust && fromJust win == O;
        then 0.01; -- we win
    else if isJust && fromJust win == X;
        then 1; -- player wins
    else;
        0.01 + $ search boardX (succ player) }),i)) newBoards
  scores 
   
-- find all possible moves I can make, then add up,
--  for all subsequent moves, how many end in losses.
-- pick the result with the smallest number
-- every turn adds 0.01, losing adds 1, winning adds 0. 
-- This pushes it to finish quickly, if that is possible    



helpHelp = " 0 | 1 | 2" ++ line ++ " 3 | 4 | 5" ++ line ++ " 6 | 7 | 8"  

-- if the user doesn't input an int, or the conditional returns false
-- re-prompt 
-- this string is the help, and will be printed when (?) is printed
getIntInput :: String -> (Int -> Bool) -> String -> IO Int
getIntInput message conditional help = do
        putStr message
        hFlush stdout        
        move <- getLine
        if move == "?"
        then do
            putStrLn help
            getIntInput message conditional help
        else do
            let maybeInt = ((readMaybe move) :: Maybe Int) -- todo: remove ()
            if maybe False conditional maybeInt 
            then do 
                return $ fromJust maybeInt
            else getIntInput message conditional help 

data GameWinner = Tie | GameWinner Player

gameStep :: Board -> Player -> AIMode -> IO GameWinner
gameStep board player mode = do
  putStrLn $ show player ++ "'s turn"
  newBoard <- playerMove board player mode
  let isWin = win newBoard
  if isNothing isWin
       then gameStep newBoard (succ player) mode
  else return $ fromJust isWin
 
main = do
    putStr "Play agaist Computer or Player 2? (C/p): "
    hFlush stdout
    picked <- getLine
    let mode = if picked == "p" then PvP else PvC
    putStrLn $ "Mode: " ++ (show mode)
    result <- gameStep emptyBoard X mode
    case result of
      (Tie) -> putStrLn $ "It was a tie!"
      (GameWinner p) -> putStrLn $ "Player " ++ show p ++ " won!!"



