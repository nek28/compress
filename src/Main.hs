module Main where

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Text.Parse
import Data.List

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

--fuses together two or more repeat blocks that are nested inside each other
--example: 2<3<4<xx>>> would become 24<xx>
transform :: Compressed -> Compressed
transform (Chain l) = Chain (trns' l)
    where
        trns' = foldr step []
        step cfact ls =
            case cfact of
                TextFactor t -> cfact : ls
                Repeat n nested -> case transform nested of
                                    Chain ([Repeat m text]) -> (Repeat (n*m) text) : ls
                                    anythingelse            -> (Repeat n anythingelse) : ls

decompress :: Compressed -> Text
decompress (Chain ls) = foldl' step T.empty ls
    where
        step :: Text -> CFactor -> Text
        step base factor = 
                case factor of
                    TextFactor t  -> base <> t
                    Repeat n nest -> base <> (T.replicate n (decompress nest))

getResult :: Parser a -> Text -> a
getResult pA inp = case parse pA inp of
                        [(result, mempty)]  -> result
                        _                   -> error "parsing failed, invalid value"

main :: IO ()
main = do
    str <- TIO.getLine
    let comprValue = getResult readCompr str
    print (transform comprValue)
    TIO.putStrLn $ "the result is: " <> (decompress . transform) comprValue
    main