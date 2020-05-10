import Prelude hiding (lex)

import Data.Maybe
import Data.List 


data Token =  OpenLambda |
              CloseLambda |
              IntToken Int |
              IdentifierToken String |
              OperatorToken Char |
              LambdaToken [Token] deriving (Show, Eq)


lex :: String -> [Token]
lex ('{':xs) = OpenLambda  : lex xs
lex ('}':xs) = CloseLambda : lex xs
lex (' ':xs) = lex xs



lex (x:xs) 
  | x `elem` ['0'.. '9'] = IntToken (read [x] :: Int) : lex xs
  | x `elem` "+-" = OperatorToken x : lex xs
  | otherwise = do
    -- take tokens till it's not in letters
    let split = indexOf (not . (`elem` letters)) xs
    case split of
      Just i -> do
          let id = x : take (i - 1) xs
          let rest = drop (i - 1) xs
          IdentifierToken id : lex rest
      Nothing -> [IdentifierToken $ x : xs]
lex "" = []


parse :: [Token] -> [Token]
parse x = do
    let (inside, outside) = parseLambda x
    if outside == [] then inside else error "hmm this didn't work"

-- assumes it ends with an end-lambda token but does not start with one
parseLambda :: [Token] -> ([Token], [Token])
-- returns what's inside and what's left
parseLambda (CloseLambda:xs) = ([], xs)
parseLambda (OpenLambda:xs) = do
    let (inside0, outside0) = parseLambda xs
    -- parse the inside lambda
    let lambda = LambdaToken inside0
    let (inside1, outside1) = parseLambda outside0
    (lambda : inside1, outside1)

parseLambda (x:xs) = do
    let (inside, outside) = parseLambda xs
    (x : inside, outside)

parseLambda [] = ([], [])


letters = ['a' .. 'z']

indexOf :: (a -> Bool) -> [a] -> Maybe Int
indexOf f [] = Nothing
indexOf f (x:xs) = if f x then Just 1 else fmap (+1) $ indexOf f xs


data Value = LambdaValue [Token] | IntValue { getIntValue :: Int } deriving (Eq)

instance Show Value where
    show (IntValue x) = show x
    show (LambdaValue xs) = show xs


data State = State [Value] [(Char, Value)]



joinToString :: Show a => [a] -> (String, String, String) -> String
joinToString xs (start, sep, end) = foldr (\a b -> if b == start then start ++ show a else b ++ sep ++ show a) start xs ++end

instance Show State where
    show (State values map) = do
        " -- State -- " ++ joinToString (reverse values) ("\n","\n","\n") ++ joinToString betterMap ("","\n","")
            where betterMap = filter (\(_, value) -> keep value) map
                  keep (IntValue 0) = False
                  keep x = True


emptyState = State [] [(c, IntValue 0) | c <- ['a'.. 'z']]

push :: State -> Value -> State 
push (State xs map) value = State (value:xs) map

pop :: State -> (Value, State)
pop (State (x:xs) map) = (x, State xs map)

getRegister :: State -> Char -> Value
getRegister (State _ map) char = second $ head $ filter (\(c,_) -> c==char) map 

setRegister :: State -> Char -> Value -> State
setRegister (State xs map) char value = State xs $ (char, value) : filter (\(c,_) -> c/=char) map


second (_, b) = b
first (a, _) = a

run :: String -> IO State
run x = run' (parse . lex $ x) emptyState

lower = "abcdefghijklmnopqrstuvwxyz"
upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

toLower :: Char -> Char
toLower c = (!!) lower $ fromJust $ elemIndex c upper

getLambdaValue :: Value -> [Token]
getLambdaValue (LambdaValue xs) = xs











run' :: [Token] -> State -> IO State
-- mutates the stack based on all of the tokens
run' [] state = return state
run' (IntToken x:xs) state = run' xs $ push state (IntValue x) 

run' (OperatorToken x:xs) state0 | x `elem` "+-/*" = do
    let (op1, state1) = pop state0
    let (op2, state2) = pop state1
  
    let (_, operator) = head $ filter (\(a,b) -> a == x) $ zip "+-/*" [(+),(-), (div),(*)]

    run' xs $ push state2 $ IntValue (getIntValue op2 `operator` getIntValue op1)
      | otherwise = error $ "idk the token " ++ show x

run' (IdentifierToken [x]:xs) state0 
  | x `elem` "abcdefghijklmnopqrstuvwxyz" =  run' xs $ push state0 $ getRegister state0 x  -- push a
  | x `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ" =  do -- pop a
      let (value, state1) = pop state0
      let state2 = setRegister state1 (toLower x) value
      run' xs state2


run' (IdentifierToken "if":xs) state0 = do
                          let (op1, state1) = pop state0
                          let (op2, state2) = pop state1
                          run' xs $ if getIntValue (getRegister state2 'a') == 0 then push state2 op2 else push state2 op1 

run' (IdentifierToken "run":xs) state0 = do
                          let (value, state1) = pop state0
                          run' (getLambdaValue value) state1 >>= run' xs
run' (IdentifierToken "out":xs) state0 = do
                          let (value, state1) = pop state0
                          print value >> (run' xs state1)

run' (IdentifierToken "in":xs) state0 = do
                          putStr "> "
                          i <- readLn 
                          run' xs $ push state0 (IntValue i)

run' (LambdaToken tokens:xs) state = run' xs $ push state $ LambdaValue tokens 

 


