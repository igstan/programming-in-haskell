import Char (ord, chr, isLower)

halve :: [a] -> ([a], [a])
halve xs = splitAt middle xs
    where middle = length xs `div` 2

safetail :: [a] -> [a]
safetail []     = []
safetail (x:xs) = xs

safetail' :: [a] -> [a]
safetail' xs = if null xs then xs else tail xs

safetail'' :: [a] -> [a]
safetail'' xs | null xs   = xs
              | otherwise = tail xs

(.||.) :: Bool -> Bool -> Bool
True .||.  _ = True
False .||. b = b

(.&&.) :: Bool -> Bool -> Bool
-- a .&&. b = if a then if b then True else False else False
a .&&. b = if a then b else False

mult :: Num a => a -> a -> a -> a
mult = \x -> \y -> \z -> x * y * z

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n char | isLower char = int2let ((let2int char + n) `mod` 26)
             | otherwise = char

-- Caesar's cipher
encode :: Int -> String -> String
encode factor xs = [shift factor x | x <- xs]

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count char inString = length [c | c <- inString, c == char]

letterFreqs :: [Float]
letterFreqs = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
               6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs

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
    where factor   = head (positions (minimum chiTable) chiTable)
          chiTable = [chiSquare (rotate n table') letterFreqs | n <- [0..25]]
          table'   = freqs xs

sumOfSquares :: Int -> Int
sumOfSquares n = sum [x^2 | x <- [1..n]]

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

pythagoreanNumbers :: Int -> [(Int, Int, Int)]
pythagoreanNumbers n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (init (factors x))]

test :: [(Int, Int)]
test = [(x, y) | x <- [1,2,3], y <- [4,5,6]]

scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]
