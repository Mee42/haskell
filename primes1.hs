odds = [x | x <- [2..],odd x]

containsDiviser :: Int -> [Int] -> Bool
containsDiviser a b = any (\x -> a `mod` x == 0) b

mapOutSecond :: [(a,b)] -> [b]
mapOutSecond a = (\(x,y) -> y) `map` a 

primes = mapOutSecond [(index,v) | (index,v) <- (zip [0..] odds),not (v `containsDiviser` (take index odds))]


