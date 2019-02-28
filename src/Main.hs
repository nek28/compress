module Main where

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Text.Parse
import Data.List

intlist :: Parser [Int]
intlist = int `sepby1` text ", "

manychars :: Parser Text
manychars = manyText item

---------------------------------

data CFactor = TextFactor Text
             | Repeat Int Compressed
    deriving (Show)
data Compressed = Chain [CFactor]
    deriving (Show)

---------------------------------

--takes a valid string and turns it into a value of type Compressed
readCompr :: Parser Compressed
readCompr = many1 (txt <|> repeatBlock) |- (result . Chain)

txt = manyText1 lower |- (result . TextFactor)
repeatBlock = nat |- (\n ->
                brackets (char '<') readCompr (char '>') |- (\compr ->
                        result $ Repeat n compr))

decompress :: Compressed -> Text
decompress (Chain ls) = foldl' step T.empty ls
    where
        step :: Text -> CFactor -> Text
        step base factor = 
                case factor of
                    TextFactor t  -> T.append base t
                    Repeat n nest -> T.append base (T.replicate n (decompress nest))

getResult :: Parser a -> Text -> a
getResult pA inp = case parse pA inp of
                        [(result, mempty)]  -> result
                        _                   -> error "parsing failed, invalid value"

main :: IO ()
main = do
    str <- TIO.getLine
    let comprValue = getResult readCompr str
    print comprValue
    TIO.putStrLn $ "the result is: " <> decompress comprValue
    main