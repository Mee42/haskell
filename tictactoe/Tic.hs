import Text.Read

data Player = X | O
  deriving (Eq,Show,Enum) -- hack till lucid teachs me how tf types work 

next :: Player -> Player
next O = X
next X = O

type Square = Maybe Player


data Board = Board [Square]



toStringS :: Square -> String
toStringS (Just X) = "X"
toStringS (Just O) = "O"
toStringS Nothing = "_"

toStringR :: Board -> Int -> String
toStringR (Board a) x = " " ++ (toStringS (a !! (3*x + 0))) ++ " | " ++ (toStringS (a !! (3*x+1))) ++ " | " ++ (toStringS (a !! (3*x+2)))

line = "\n------------\n"

toStringB :: Board -> String
toStringB a = toStringR a 0 ++ line ++ (toStringR a 1) ++ line ++ (toStringR a 2)

getPlayer :: Square -> Player
getPlayer (Just a) = a

winRow :: Board -> (Int,Int,Int) -> Maybe Player
winRow (Board a) (x,y,z) = if (a !! x) == (a !! y) && (a !! x) == (a !! z) && ((a !! x) /= Nothing)
                        then Just $ getPlayer (a !! x)
                        else Nothing

combine :: Maybe Player -> Maybe Player -> Maybe Player
combine (Just a) (Just b)= Just a
combine (Just a) Nothing = Just a
combine Nothing (Just b) = Just b
combine Nothing Nothing  = Nothing

combineList :: [Maybe Player] -> Maybe Player
combineList list = foldl combine Nothing list

win :: Board -> Maybe Player
win a = combineList $ map (winRow a) [(0,1,2),(3,4,5),(6,7,8),(0,3,6),(1,4,7),(2,5,8),(0,4,8),(2,4,6)]


play :: Board -> Player -> Int -> Maybe Board
play (Board a) player index = if index < 0 || index > 8 || (a !! index) /= Nothing then Nothing
                              else Just $ Board $ take index a ++ [Just player] ++ drop (index + 1) a


emptyBoard = Board [ Nothing | x <- [0..8]]
 
playTurn :: Board -> Player -> IO ()
playTurn board player = do
                            putStrLn $ toStringB board
                            putStrLn $ "Player " ++ show player ++ " turn"
                            putStr "Move:"
                            move <- getLine


-- if the user doesn't input an int, or the conditional returns false
-- re-prompt 
getIntInput :: String -> (Int -> Boolean) -> IO Int
getIntInput message conditional = do
        putStr message
        move <- getLine
        maybeInt <- (readMaybe move :: Int)
        if maybe False conditional maybeInt 
        then return getIntInput message conditional
        else return maybeInt          
                           
main = playTurn emptyBoard X






