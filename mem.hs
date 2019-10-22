



mem :: [((Int -> b) -> Int -> b)] -> (Int -> b)
mem funs = (!!) (map ((head funs) (mem (drop 1 funs))) [0..]) 

mem_limit :: ((Int -> b) -> Int -> b) -> Int -> (Int -> b)
mem_limit fun limit = mem [fun | x <- [0..limit]]

-- the function you would define somehow inline (where thing?)
mem_fib_temp :: (Int -> Int) -> Int -> Int
mem_fib_temp f 0 = 0
mem_fib_temp f 1 = 1
mem_fib_temp f x = f(x-2) + f(x-1)

-- to use, call 
xx = mem_limit mem_fib_temp 100 -- 100 is the limit


-- here's an inline version
yy = mem_limit (\f x -> 
   do
     case x of
         0 -> 0
         1 -> 1
         x -> f(x-2) + f(x-1)) 100

-- s l o w
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib(x-1) + fib(x-2)
