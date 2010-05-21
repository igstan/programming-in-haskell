import Data.Char (chr, ord)

squaresOfEvens :: Integral a => [a] -> [a]
squaresOfEvens = map (^ 2) . filter even

-- all even [2,4,6,8]
-- True
all' :: (a -> Bool) -> [a] -> Bool
all' f = and . map f

-- any odd [2,4,6,8]
-- False
any' :: (a -> Bool) -> [a] -> Bool
any' f = or . map f

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                 = []
takeWhile' f (x:xs) | f x       = x : takeWhile' f xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []                 = []
dropWhile' f (x:xs) | f x       = dropWhile' f xs
                    | otherwise = x:xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x ys -> f x : ys) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\ x ys -> if f x then x:ys else ys) []

-- dec2int [2,3,4,5]
-- 2345
-- 2*1000 + 3*100 + 4*10 + 5*1
-- (2*100 + 3*10 + 4*1)*10 + 5*1
-- ((2*10 + 3)*10 + 4)*10 + 5
dec2int :: [Int] -> Int
dec2int = foldl (\a b -> a * 10 + b) 0

compose :: [(a -> a)] -> (a -> a)
compose = foldr (.) id

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \a b -> f (a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(a, b) -> f a b

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Int]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

type Bit = Int

bin2int :: [Bit] -> Int
-- bin2int bits = sum [w * b | (w, b) <- zip weights bits]
--                where weights = iterate (*2) 1
-- bin2int bits = sum $ zipWith (*) weights bits
--                where weights = iterate (*2) 1
bin2int = foldr (\x y -> x + 2 * y) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

make9 :: [Bit] -> [Bit]
make9 bits = bits' ++ [parity]
             where bits'  = make8 bits
                   parity = if even (sum bits') then 0 else 1

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = first8 : chop8 (drop 9 bits)
             where parity = take 1 (drop 8 bits)
                   first8 = if parity == [0] && even (sum (take 8 bits)) then
                                take 8 bits
                            else
                                error "Parity check failed"

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

encode :: String -> [Bit]
encode = concat . map (make9 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . id . encode
