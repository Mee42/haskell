module Lib (Type(SingleType, IntType, AddedType, EitherType),
            Expression(IntExpression),
            typeOf) where



--import Text.Parsec.String (Parser)
--import Text.Parsec.String.Char (anyChar)
--import Text.Parsec.String.Char
import Text.Parsec
import Data.Char
--import Text.Parsec.String.Combinator (many1)

data Type = SingleType Int |
            IntType |
            AddedType Type Int | 
            EitherType Type Type

instance Show Type where
    show (SingleType i) = show i
    show (IntType) = "int"
    show (AddedType typ i) = show typ ++ " + " ++ show i



data Expression =  IntExpression Int

instance Show Expression where
    show (IntExpression i) = show i

typeOf :: Expression -> Type
typeOf (IntExpression i) = SingleType i


--num :: Parser Int
--num = do
--    n <- many1 digit
--    return (read n)
--
