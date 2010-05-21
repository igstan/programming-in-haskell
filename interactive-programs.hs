import Data.Char (isDigit, isLower, isUpper, isAlpha, isAlphaNum, isSpace)

type Parser a = String -> [(a, String)]

return' :: a -> Parser a
return' v = \input -> [(v, input)]

failure :: Parser a
failure = \input -> []

item :: Parser Char
item = \input -> case input of
                       []     -> []
                       (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse parser input = parser input

-- Sequencing operator
(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>>= f = \input -> case parse p input of
                     []         -> []
                     [(v, out)] -> parse (f v) out

p :: Parser (Char, Char)
p = item >>>= \x ->
    item >>>= \_ ->
    item >>>= \y ->
    return' (x, y)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \input -> case parse p input of
                         []         -> parse q input
                         [(v, out)] -> [(v, out)]

satisfies :: (Char -> Bool) -> Parser Char
satisfies predicate = item >>>= \x -> if predicate x then return' x else failure

digit :: Parser Char
digit = satisfies isDigit

lower :: Parser Char
lower = satisfies isLower

upper :: Parser Char
upper = satisfies isUpper

letter :: Parser Char
letter = satisfies isAlpha

alphanum :: Parser Char
alphanum = satisfies isAlphaNum

char :: Char -> Parser Char
char x = satisfies (== x)

string :: String -> Parser String
string []     = return' []
string (x:xs) = char x    >>>= \_ ->
                string xs >>>= \_ ->
                return' (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return' []

many1 :: Parser a -> Parser [a]
many1 p = p      >>>= \v  ->
          many p >>>= \vs ->
          return' (v:vs)

ident :: Parser String
ident = lower         >>>= \x ->
        many alphanum >>>= \xs ->
        return' (x:xs)

nat :: Parser Int
nat = many1 digit >>>= \xs ->
      return' (read xs)

space :: Parser ()
space = many (satisfies isSpace) >>>= \_ ->
        return' ()

token :: Parser a -> Parser a
token p = space >>>= \_ ->
          p     >>>= \v ->
          space >>>= \_ ->
          return' v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

list :: Parser [Int]
list = symbol "[" >>>= \_ ->
       natural    >>>= \n ->
       many (symbol "," >>>= \_ -> natural) >>>= \ns ->
       symbol "]" >>>= \_ ->
       return' (n:ns)

-- Arithmetic BNF grammar
-- expr ::= expr + expr | expr * expr | (expr) | nat
-- nar  ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

-- expr   ::= term ( + expr | - expr | '')
-- term   ::= factor (* term | / term | '')
-- factor ::= (expr) | nat
-- nat    ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

-- expr   ::= term ( + expr | - expr | '')
-- term   ::= expo (* term | / term | '')
-- factor ::= atom (^ factor | '')
-- atom   ::= (expr) | nat
-- nat    ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

expr :: Parser Int
expr = term >>>= \t ->
        ((symbol "+" >>>= \_ ->
          expr       >>>= \e ->
          return' (t + e))
         +++
         (symbol "-" >>>= \_ ->
          expr       >>>= \e ->
          return' (t + e)))
        +++ return' t

term :: Parser Int
term = factor >>>= \f ->
        ((symbol "*" >>>= \_ ->
          term       >>>= \t ->
          return' (f * t))
         +++
         (symbol "/" >>>= \_ ->
          term       >>>= \t ->
          return' (f `div` t)))
        +++ return' f

factor :: Parser Int
factor = atom >>>= \a ->
         (symbol "^" >>>= \_ ->
          factor     >>>= \f ->
          return' (a ^ f))
          +++ return' a

atom :: Parser Int
atom = (symbol "(" >>>= \_ ->
        expr       >>>= \e ->
        symbol ")" >>>= \_ ->
        return' e)
       +++ natural

int :: Parser Int
int = (symbol "-" >>>= \_ ->
       nat        >>>= \n ->
       return' (-n))
      +++ nat

comment :: Parser ()
comment = symbol "--"                >>>= \_ ->
          many (satisfies (/= '\n')) >>>= \_ ->
          char '\n'                  >>>= \_ ->
          return' ()

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                  return []
               else
                  do xs <- getLine'
                     return (x:xs)

putStr' :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = do putChar x
                    putStr xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr xs
                  putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

beep :: IO ()
beep = putStr "\BEL"

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt pos xs = do goto pos
                    putStr xs

seqn :: [IO a] -> IO ()
seqn []     = return ()
seqn (a:as) = do a
                 seqn as

putStr'' :: String -> IO()
putStr'' xs = seqn [putChar x | x <- xs]

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: [Char]
buttons  = standard ++ extra
           where standard = "qcd=123+456-789*0()/"
                 extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [writeAt (1,y) xs | (y,xs) <- zip [1..13] box]

display :: String -> IO ()
display xs = do writeAt (3,2) "             "
                writeAt (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getChar
             if elem c buttons then
                 process c xs
              else
                  do beep
                     calc xs

process :: Char -> String -> IO ()
process c xs
    | elem c "qQ\ESC"    = quit
    | elem c "dD\BS\DEL" = delete xs
    | elem c "=\n"       = eval xs
    | elem c "cC"        = clear
    | otherwise          = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
               [(n,"")] -> calc (show n)
               _        -> do beep
                              calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do clearScreen
         showbox
         clear
