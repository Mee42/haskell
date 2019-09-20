import Text.Read
import Data.Maybe
import System.IO


data Player = X | O
    deriving (Eq,Show)

instance Enum Player where
    succ x = case x of
               X -> O
               O -> X
    toEnum x = if x `mod` 2 == 0 then X else O 
    fromEnum x = if x == X then 0 else 1


newtype Square = Square (Maybe Player)

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
play (Board a) player index = if index < 0 || index > 8 || (a !! index) /= Nothing then Nothing
                              else Just $ Board $ take index a ++ [Just player] ++ drop (index + 1) a


emptyBoard = Board [ Square Nothing | x <- [0..8]]
 
playerMove :: Board -> Player -> IO Board
playerMove board player = do
                            putStrLn $ show board
                            putStrLn $ "Player " ++ show player ++ " turn"
                            move <- getIntInput "Move (0..8): " (\x -> x `elem` [0..8]) helpHelp
                            let new = play board player move
                            putStrLn $ maybe "invalid move" show new
                            if isJust new
                            then return $ fromJust new
                            else do
                              playerMove board player


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

gameStep :: Board -> Player -> IO GameWinner
gameStep board player = do
  putStrLn $ show player ++ "'s turn"
  newBoard <- playerMove board player
  let isWin = win newBoard
  if isNothing isWin
       then gameStep newBoard (succ player)
  else return $ fromJust isWin
 
main = do
    result <- gameStep emptyBoard X
    case result of
      (Tie) -> putStrLn $ "It was a tie!"
      (GameWinner p) -> putStrLn $ "Player " ++ show p ++ " won!!"



