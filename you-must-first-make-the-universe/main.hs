{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Prelude as P (Char, error)
import Prelude (($), (.))
data Nat = Z | S Nat

n0 = Z
n1 = S n0
n2 = S n1
n3 = S n2
n4 = S n3
n5 = S n4
n6 = S n5
n7 = S n6
n8 = S n7
n9 = S n8
n10 = S n9

data Bool = True | False

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ x = x


data List a = Cons a (List a) | Nil

type String = List P.Char

helloWorld = Cons 'H' $ Cons 'e' $ Cons 'l' $ Cons 'l' $ Cons 'o' $ Cons '!' Nil


class Show a where
    show :: a -> String

_string :: String -> [P.Char]
_string (Cons char xs) = char : _string xs
_string (Nil) = []

_print :: Show a => a -> [P.Char]
_print = _string . show

_from_str :: [P.Char] -> String
_from_str (x:xs) = Cons x (_from_str xs)
_from_str [] = Nil

instance Show Nat where
    show Z = Cons '0' Nil
    show (S x) = incrStr (show x)


instance Show String where
    show x = x

instance Show Bool where
    show True  = Cons 'T' $ Cons 'r' $ Cons 'u'            $ Cons 'e' Nil
    show False = Cons 'F' $ Cons 'a' $ Cons 'l' $ Cons 's' $ Cons 'e' Nil

incrementReversedNumber :: String -> String
incrementReversedNumber (Cons '0' xs) = Cons '1' xs
incrementReversedNumber (Cons '1' xs) = Cons '2' xs
incrementReversedNumber (Cons '2' xs) = Cons '3' xs
incrementReversedNumber (Cons '3' xs) = Cons '4' xs
incrementReversedNumber (Cons '4' xs) = Cons '5' xs
incrementReversedNumber (Cons '5' xs) = Cons '6' xs
incrementReversedNumber (Cons '6' xs) = Cons '7' xs
incrementReversedNumber (Cons '7' xs) = Cons '8' xs
incrementReversedNumber (Cons '8' xs) = Cons '9' xs
incrementReversedNumber (Cons '9' Nil) = Cons '0' (Cons '1' Nil)
incrementReversedNumber (Cons '9' xs) = Cons '0' (incrementReversedNumber xs)


(++) :: String -> String -> String
(++) a Nil = a
(++) Nil a = a
(++) (Cons x xs) ax = Cons x $ xs ++ ax

reverse :: String -> String
reverse Nil = Nil
reverse (Cons x xs) = reverse xs ++ Cons x Nil

incrStr :: String -> String
incrStr = reverse . incrementReversedNumber . reverse

class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a

infixl 7 *
infixl 7 /
infixl 6 +
infixl 6 -

instance Num Nat where
    (+) a Z = a
    (+) Z a = a
    (+) a (S x) = S (a + x)
    
    (-) a Z = a
    (-) Z _ = P.error "fuck you, negative numbers literally don't exist"
    (-) (S a) (S b) = a - b

    (*) Z _ = Z
    (*) _ Z = Z
    (*) (S a) b = b + (a * b)

    (/) Z _ = Z
    (/) _ Z = P.error "fuck you, division by zero"
    (/) (S Z) (S Z) = S Z
    (/) (S Z) x = Z -- round down here
    (/) a b = if' (S a < b) Z ((a - b) / b + S Z)


(<) :: Nat -> Nat -> Bool
(<) Z Z = False
(<) Z x = True
(<) x Z = False
(<) (S a) (S b) = a < b






