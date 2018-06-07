module Caesar
    (crackCaesar)
    where

import Data.Char

{-
Sample program from Chapter 5 of the book "Programming in Haskell"
The program decodes a string which was encoded using the Caesar cipher.
I extended this program by dynamically calculating the characters frequency
table for the english language, instead of using a precompiled one.
-}

sampleEnglishText :: String
sampleEnglishText = "This is a paragraph that uses every single letter in the alphabet \
 \ Now that doesn t mean this can be a paragraph with no story but it does mean that \
 \ every single letter is used You can make it as generic or fanciful as you d like \
 \ You can talk about anything from quilts to jets to xylophones Oh yeah and you can use \
 \ whatever language you want from Afrikaans to Zulu"

alphabet :: [Char]
alphabet =  ['a'..'z']

countChar :: String -> Char -> Int
countChar s c = length $ filter (== c) s

percent :: Int -> Int -> Float
percent a b = (fromIntegral a / fromIntegral b) * 100

stringToLowerCase :: String -> String
stringToLowerCase s = [toLower c | c <- s]

frequencyTable :: String -> [Float]
frequencyTable s = [percent (countChar lowerCaseString c) totalLength | c <- alphabet]
    where
      lowerCaseString = stringToLowerCase s
      totalLength = length s

-- Return a list of positions where x appears in xs
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [p | (i, p) <- (zip xs [0..]), i == x]

let2Int :: Char -> Int
let2Int c = ord c - ord 'a'

int2Let :: Int -> Char
int2Let i = chr (ord 'a' + i)

shift :: Char -> Int -> Char
shift c i
  | isLower c = int2Let (((let2Int $ toLower c ) + i) `mod` 26)
  | otherwise = c

encode :: String -> Int -> String
encode s f = [shift e f | e <- s]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crackCaesar :: String -> String
crackCaesar xs = encode xs (-factor)
    where
      factor = head (positions (minimum chitab) chitab)
      chitab = [chisqr (rotate n xStringTable) englishFrequencyTable | n <- [0..25]]
      xStringTable = frequencyTable xs
      englishFrequencyTable = frequencyTable sampleEnglishText
