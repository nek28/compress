{-# LANGUAGE OverloadedStrings #-}
module Text.Parse where

import           Data.Text (Text)
import qualified Data.Text as T

newtype Parser a = Parser { parse :: Text -> [(a,Text)] }

item :: Parser Char
item = Parser $ \inp -> if inp == T.empty
                        then []
                        else [(T.head inp, T.tail inp)]

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

nat :: Parser Int
nat = manyText digit |- (result . eval)
    where
        eval :: Text -> Int
        eval = T.foldl' (\base num -> (base * 10) + (toInt num)) 0
        toInt c = fromEnum c - fromEnum '0'

int :: Parser Int
int = neg |- (\f ->
            nat |- (\n ->
                result $ f n))
    where
        neg = minus <|> result id
        minus = char '-' |- (\_ -> result (negate))

word = plusOneLetter <|> result T.empty
    where
        plusOneLetter = letter |- (\l ->
                                word |- (\rest ->
                                        result (T.cons l rest)))

manyText :: Parser Char -> Parser Text
manyText pC = oneCharFurther <|> result T.empty
    where
        oneCharFurther = pC |- (\firstChar ->
                                manyText pC |- (\rest ->
                                        result $ T.cons firstChar rest))

many :: Parser a -> Parser [a]
many pA = oneStepFurther <|> result []
    where
        oneStepFurther = pA |- (\first ->
                                many pA |- (\rest ->
                                         result $ first : rest)) 

text :: Text -> Parser Text
text t = if t == T.empty
         then result (T.empty)
         else stepFurther
    where
        stepFurther = char (T.head t) |- (\first ->
                            text (T.tail t) |- (\rest ->
                                    result $ T.cons first rest))

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 pA pSep = pA |- (\first ->
                        many (pSep |- (\_ -> pA |- result)) |- (\rest ->
                                result $ first : rest))