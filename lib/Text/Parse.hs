{-# LANGUAGE OverloadedStrings #-}
module Text.Parse where

import           Data.Text (Text)
import qualified Data.Text as T

newtype Parser a = Parser { parse :: Text -> [(a,Text)] }

item :: Parser Char
item = Parser $ \inp -> [(T.head inp, T.tail inp)]

(|-) :: Parser a -> (a -> Parser b) -> Parser b
pA |- fTopB = Parser $ \inp -> concatMap (\(result, rest) -> parse (fTopB result) rest) (parse pA inp)

result :: a -> Parser a
result v = Parser $ \inp -> [(v, inp)]

zero :: Parser a
zero = Parser $ \inp -> []

plus :: Parser a -> Parser a -> Parser a
plus pA pB = Parser $ \inp -> (parse pA inp ++ parse pB inp)

--first can be applied to a parser that always succeeds,
--and takes its first result. This makes the parser deterministic
first :: Parser a -> Parser a
first pA = Parser $ \inp -> case parse pA inp of
                                [] -> []
                                (x:xs) -> [x]

(<|>) :: Parser a -> Parser a -> Parser a
pA <|> pB = first (pA `plus` pB)

sat :: (Char -> Bool) -> Parser Char
sat pred = item |- (\c1 -> if pred c1
                           then result c1
                           else zero)

char :: Char -> Parser Char
char c = sat (== c)

digit = sat (\c -> c >= '0' && c <= '9')
lower = sat (\c -> c >= 'a' && c <= 'z')
upper = sat (\c -> c >= 'A' && c <= 'Z')

letter = lower <|> upper
alphanum = letter <|> digit

word = plusOneLetter <|> result T.empty
    where
        plusOneLetter = 