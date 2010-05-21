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

-- p :: Parser (Char, Char)
-- p = do x <- item
--        item
--        y <- item
--        return' (x, y)

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

eval :: String -> Int
eval xs = case parse expr xs of
               [(n, [])]  -> n
               [(_, out)] -> error ("unused input " ++ out)
               []         -> error "invalid input"

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

-- expr ::= expr - nat | nat
-- nat  ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

nexpr :: Parser Int
nexpr = (nexpr      >>>= \e ->
         symbol "-" >>>= \_ ->
         natural    >>>= \n ->
         return' (e - n))
        +++ natural

many :: Parser a -> Parser [a]
many p = many1 p +++ return' []

many1 :: Parser a -> Parser [a]
many1 p = p      >>>= \v  ->
          many p >>>= \vs ->
          return' (v:vs)

nexpr' :: Parser Int
nexpr' = natural                              >>>= \n ->
         many (symbol "-" >>>= \_ -> natural) >>>= \ns ->
         return' (foldl (-) n ns)
