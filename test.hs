product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple = double . double

last' = head . reverse
last'' xs = xs !! (length xs - 1)

init' = reverse . tail . reverse
init'' xs = take (length xs - 1) xs

swap (x,y) = (y,x)

pair x y = (x,y)

palindrome xs = reverse xs == xs

twice f = f . f
