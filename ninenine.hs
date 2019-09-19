
-- 1
myLast :: [a] -> a
myLast = head . reverse


-- 2
myButLast :: [a] -> a
myButLast = last . init

-- 3
elementAt :: [a] -> Int -> a
elementAt arr ind = (!!) arr $ ind - 1

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:arr) = myLength arr + 1

--  5
myReverse :: [a] -> [a]
myReverse (a:ex) = myReverse ex ++ [a] 
myReverse [] = []

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (ex) = (head ex) == (last ex) && isPalindrome (init $ tail $ ex)
