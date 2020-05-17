import Prelude hiding (lex)

import Data.Maybe
import Data.List 
import Control.Monad.State (get, put, StateT, runStateT, liftIO)

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
lex ('\n':xs) = lex xs


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


letters = '_':['a' .. 'z']

indexOf :: (a -> Bool) -> [a] -> Maybe Int
indexOf f [] = Nothing
indexOf f (x:xs) = if f x then Just 1 else fmap (+1) $ indexOf f xs


data Value = LambdaValue [Token] | IntValue { getIntValue :: Int } deriving (Eq)

instance Show Value where
    show (IntValue x) = show x
    show (LambdaValue xs) = show xs


data State = State [Value] [(Char, Value)] [(String, [Token])] 



joinToString :: Show a => [a] -> (String, String, String) -> String
joinToString xs (start, sep, end) = foldr (\a b -> if b == start then start ++ show a else b ++ sep ++ show a) start xs ++end

instance Show State where
    show (State values map vars) = do
        " -- State -- " ++ 
            joinToString (reverse values) ("\n","\n","\n") ++ 
                joinToString betterMap ("","\n","") ++ variables
            where betterMap = filter (\(_, value) -> keep value) map
                  keep (IntValue 0) = False
                  keep x = True
                  stringVars = StringLit <$> (\(name, tokens) -> name ++ ": " ++ show tokens) <$> vars 
                  variables | vars == [] = ""
                            | otherwise = joinToString stringVars (" -- variables -- \n","\n","")


data StringLit = StringLit String

instance Show StringLit where
    show (StringLit s) = s


emptyState = State [] [(c, IntValue 0) | c <- ['a'.. 'z']] []

run_ :: String -> IO State
run_ x = snd <$> runStateT (run.parse.lex $ x) emptyState 

runf :: String -> IO State
runf p = readFile p >>= run_

lower = "abcdefghijklmnopqrstuvwxyz"
upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

toLower :: Char -> Char
toLower c = (!!) lower $ fromJust $ elemIndex c upper

getLambdaValue :: Value -> [Token]
getLambdaValue (LambdaValue xs) = xs

unwrap :: State -> ([Value], [(Char, Value)], [(String, [Token])])
unwrap (State xs rx vx) = (xs, rx, vx)

-- runAll :: String -> IO State

push :: Value -> StateT State IO ()
push value = do
    (values, regs, vars) <- unwrap <$> get
    put $ State (value:values) regs vars
    return ()

pop :: StateT State IO Value
pop = do
    ((v:vs), regs, vars) <- unwrap <$> get
    put $ State vs regs vars
    return v

getRegister :: Char -> StateT State IO Value
getRegister c = do
    (_, regs, _) <- unwrap <$> get
    return $ snd . head $ filter ((==c).fst) regs

setRegister :: Char -> Value -> StateT State IO ()
setRegister char value = do
    (vs, regs,vars) <- unwrap <$> get
    let newRegs = (char, value) : (filter (\(x,_) -> x /= char) regs)
    put $ State vs newRegs vars 
    return ()


run :: [Token] -> StateT State IO ()

run (IntToken x:xs) = do
    push (IntValue x)
    run xs

run (OperatorToken x:xs) = do
    let (_, operator) = head $ filter (\(a,b) -> a == x) $ zip "+-/*" [(+),(-), (div),(*)]
    op1 <- getIntValue <$> pop
    op2 <- getIntValue <$> pop
    push . IntValue $ operator op1 op2
    run xs

run (IdentifierToken [x]:xs)
    | x `elem` lower = do
        getRegister x >>= push
        run xs
    | x `elem` upper = do
        pop >>= setRegister (toLower x)
        run xs

run (IdentifierToken "if":xs) = do
    op1 <- pop
    op2 <- pop
    aReg <- getIntValue <$> getRegister 'a'
    if aReg == 0 then push op2 else push op1
    run xs

run (IdentifierToken "run":xs) = do
    tokens <- getLambdaValue <$> pop
    run tokens
    run xs

run (IdentifierToken "out":xs) = do
    i <- getIntValue <$> pop
    liftIO $ print i
    run xs

run (IdentifierToken "in":xs) = do
    liftIO $ putStr "> "
    i <- liftIO (readLn :: IO Int)
    push $ IntValue i
    run xs

run (LambdaToken tokens:xs) = do
    push $ LambdaValue tokens
    run xs


run (IdentifierToken "def":IdentifierToken id:LambdaToken tokens:xs) = do
    (vx, regs, vars) <- unwrap <$> get
    let newVars = (id,tokens):filter ((/=id).fst) vars
    put $ State vx regs newVars
    run xs


run (IdentifierToken id:xs) = do
    variables <- trd <$> unwrap <$> get
    let varsAccessable = filter ((==id).fst) variables
    let tokens=if varsAccessable /= [] 
                  then snd . head $ varsAccessable 
                  else error $ "can't find variable " ++ show id
    run tokens
    run xs 


run [] = return ()




trd (_, _, x) = x






