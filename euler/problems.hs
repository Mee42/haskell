import Debug.Trace

fibs = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

fib n = fibs !! n

isEven x = x `mod` 2 == 0

problem2 = sum $ takeWhile (\x -> x < 4000000) [x | x <- fibs,isEven x ]

isPrime :: Int -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..primeCeil k], k `mod` x == 0] else False

problem3_in :: Int
problem3_in = 13195
primeCeil :: Int -> Int
primeCeil = floor . sqrt . fromIntegral 

primeFactors :: Int -> [Int]
primeFactors n = distinct $ concat $ map (\x -> if (isPrime x) then [x] else primeFactors x) $ filter (\x -> n `mod` x == 0) [2..n] 

