module Main where

import Lib
import Text.Parsec

--import Data.Char
import Text.Parsec.String

main :: IO ()
main = print "Hello, World"


wordParser :: Parsec String st String
wordParser = many $ noneOf [' ']

secondWordParser:: Parsec String st String
secondWordParser = wordParser *> (char ' ')  *> wordParser

--wordsParser :: Parsec String st [String]
--wordsParser = [wordParser, (char ' ') *> wordParser]


twoWordsParser:: Parsec String st [String]
twoWordsParser = listfy  wordParser ((char ' ') *> wordParser)
                   where listfy a b = [a, b]
