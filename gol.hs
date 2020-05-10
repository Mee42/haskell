import Control.Concurrent

size = 55

emptyCol = [ 0 | x <- [0..size]]

emptyBoard = Board [emptyCol | x <- [0..size]]

createBoard :: [(Int, Int)] -> Board
createBoard = foldr (\pos b -> set pos 1 b) emptyBoard

glider = createBoard [(1, 2), (2, 3), (3, 3), (3, 2), (3, 1)]

gun=createBoard[(1,5),(1,6),(2,5),(2,6),(14,3),(13,3),(12,4),(11,5),(11,6),(11,7),(12,8),(13,9),(14,9),(15,6),(16,4),(17,5),(17,6),(18,6),(17,7),(16,8),(23,2),(21,3),(22,3),(21,4),(22,4),(21,5),(22,5),(23,6),(25,1),(25,2),(25,6),(25,7),(35,3),(36,3),(35,4),(36,4)]

data Board = Board { getArray :: [[Int]] }

showPixel :: Int -> Char
showPixel 0 = ' '
showPixel 1 = 'X'

box :: Int -> [String] ->  String
box chars str = do
    let bar = ['-' | x <- [0..chars]]
    let s = foldr (\b a -> a ++ "\n|" ++ b ++ "|") "" str 
    ['+'] ++ bar ++ ['+'] ++ s ++ "\n+" ++ bar ++ ['+']

pixel = putStrLn . (box $ size*2) . (split '\n') . (showBoard (return . showPixel))

showBoard :: (Int -> String) -> Board -> String
showBoard f (Board (a:b:xs)) = (join " " $ f <$> a) ++ "\n" ++ showBoard f (Board $ b:xs)
showBoard f (Board [a]) = join " " $ f <$> a
showBoard _ (Board []) = ""

instance Show Board where
    show = showBoard show 

set :: (Int, Int) -> Int -> Board -> Board
set (x, y) value (Board xs) = Board [ 
     if y == y1 then [ if x == x1 then value else elem | (elem, x1) <- zip col [0..]] else col
     | (col, y1) <- zip xs [0..]]

shiftUp    :: Board -> Board
shiftDown  :: Board -> Board
shiftRight :: Board -> Board
shiftLeft  :: Board -> Board 

shiftUp (Board (x:xs)) = Board $ xs ++ [x]
shiftDown (Board xs) = Board $ last xs : (init xs)

shiftLeft  (Board xs) = Board [cs ++ [elem] | (elem:cs) <- xs]
shiftRight (Board xs) = Board [ last cs : (init cs) | cs <- xs]


applications = [map' (*10), shiftUp, shiftUp . shiftRight, shiftRight, shiftRight . shiftDown, shiftDown, shiftDown . shiftLeft, shiftLeft, shiftLeft . shiftUp]

tick b = Board $ mapMatrix (\(live,n) -> if (live && n `elem` [2, 3]) || ((not live) && n == 3) then 1 else 0) $ 
    mapMatrix (\i -> (i >= 10, i `rem` 10)) $ 
        getArray $ foldr sum' emptyBoard ((\x -> x b) <$> applications)

sum' :: Board -> Board -> Board

sum'' :: [[Int]] -> [[Int]] -> [[Int]]
sum'' (a:ax) (b:bx) = ((\(a,b)->a+b) <$> zip a b) : sum'' ax bx
sum'' [] [] = []

sum' (Board a) (Board b) = Board $ sum'' a b

mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f xs = fmap f <$> xs

map' :: (Int -> Int) -> Board -> Board
map' f (Board (xs)) = Board $ fmap f <$> xs

main = runTick gun

runTick :: Board -> IO ()
runTick board = do
    let newBoard = tick board
    pixel newBoard
    threadDelay 100000
    runTick newBoard
    return ()


split :: Char -> String -> [String]
split c xs = case break (==c) xs of
  (ls, "") -> [ls]
  (ls, x:rs) -> ls : split c rs

join :: String -> [String] -> String
join _ [] = ""
join a xs = foldr1 (concat) xs
  where
    concat b c = b ++ a ++ c
