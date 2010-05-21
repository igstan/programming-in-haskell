import Char (ord, chr, isLower, isUpper, isAlpha, toLower)

let2int :: Char -> Char -> Int
let2int c firstLetter = ord c - ord firstLetter

int2let :: Int -> Char -> Char
int2let n firstLetter = chr (ord firstLetter + n)

shift :: Int -> Char -> Char
shift n char | isLower char = int2let (((let2int char 'a') + n) `mod` 26) 'a'
             | isUpper char = int2let (((let2int char 'A') + n) `mod` 26) 'A'
             | otherwise = char

encode :: Int -> String -> String
encode factor xs = [shift factor x | x <- xs]

letters :: String -> Int
letters xs = length [x | x <- xs, isAlpha x]

count :: Char -> String -> Int
count char inString = length [c | c <- inString, toLower c == char]

letterFreqs :: [Float]
letterFreqs = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
               6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = letters xs

chiSquare :: [Float] -> [Float] -> Float
chiSquare observedFreqs expectedFreqs = sum [(o - e)^2 / e | (o,e) <- zip observedFreqs expectedFreqs]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = length xs - 1

find :: Eq a => a -> [(a, b)] -> [b]
find key pairs = [pair | (k, pair) <- pairs, k == key]

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..n])
                  where n = length xs - 1

crack :: String -> String
crack xs = encode (-factor) xs
    where factor   = (positions (minimum chiTable) chiTable) !! 0
          chiTable = [chiSquare (rotate n table') letterFreqs | n <- [0..25]]
          table'   = freqs xs
