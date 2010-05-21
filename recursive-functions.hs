-- Recursive definition of the exponentiation operator
(.^.) :: Int -> Int -> Int
a .^. 0       = 1
a .^. (n + 1) = a * (a .^. n)

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && (and' xs)

concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0       _ = []
replicate' (n + 1) a = [a] ++ replicate' n a

(.!!.) :: [a] -> Int -> a
(x:_)  .!!.      0  = x
(x:xs) .!!. (n + 1) = xs .!!. n

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = e == x || elem' e xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Merge sort
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort first) (msort second)
            where first    = fst (halve xs)
                  second   = snd (halve xs)
                  halve xs = splitAt middle xs
                  middle   = (length xs) `div` 2

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take'  _ [] = []
take'  0 xs = xs
take' (n + 1) (x:xs) = take' n xs

last' :: [a] -> a
last' [x]    = x
last' (x:xs) = last' xs

-- Insertion sort
insert :: Ord a => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)
