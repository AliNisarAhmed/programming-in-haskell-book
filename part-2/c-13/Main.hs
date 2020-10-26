module Main where

import Control.Applicative
import Data.Char
import System.IO

main :: IO ()
main = putStrLn "Hello, Haskell!"

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inputString = p inputString

item :: Parser Char
item =
  P
    ( \inp ->
        case inp of
          [] -> []
          (x : xs) -> [(x, xs)]
    )

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P p) =
    P
      ( \inp ->
          case p inp of
            [] -> []
            [(x, xs)] -> [(f x, xs)]
      )

instance Applicative Parser where
  pure x = P (\inp -> [(x, inp)])

  -- (<*>) :: Parse (a -> b) -> Parse a -> Parse b
  (<*>) (P fab) pa =
    P
      ( \inp ->
          case fab inp of
            [] -> []
            [(f, inp')] -> parse (fmap f pa) inp'
      )

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
  where
    g x _ z = (x, z)

instance Monad Parser where
  pa >>= f =
    P
      ( \inp ->
          case parse pa inp of
            [] -> []
            [(a, inp')] -> parse (f a) inp'
      )

threeM :: Parser (Char, Char)
threeM = do
  x <- item
  item
  z <- item
  return (x, z)

instance Alternative Parser where
  empty = P $ const []
  p <|> q =
    P
      ( \inpt ->
          case parse p inpt of
            [] -> parse q inpt
            [(v, out)] -> [(v, out)]
      )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

-- variable names parser
identifier :: Parser String
identifier = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

-- Natural Number parser
nat :: Parser Int
nat = do
  xs <- some digit
  return $ read xs

space :: Parser ()
space = do
  many $ sat isSpace
  return ()

int :: Parser Int
int =
  do
    char '-'
    n <- nat
    return (- n)
    <|> nat

-- token takes a parser and returns a parser
-- such that all spaces before and after are ignored
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier2 :: Parser String
identifier2 = token identifier

natural :: Parser Int
natural = token int

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <- many $ do
    symbol ","
    natural
  symbol "]"
  return (n : ns)

---- parser for expression of the following parse tree

-- expr ::= term + expr | term ::= term ( + expr | e )   :: where e is empty string
-- term ::= factor * term | factor ::= factor (* term | e)
-- factor ::= ( expr ) | nat
-- nat ::= 0 | 1 | 2 ...

expr :: Parser Int
expr = do
  t <- term
  do
    symbol "+"
    e <- expr
    return $ t + e
    <|> return t

term :: Parser Int
term = do
  f <- factor
  do
    symbol "*"
    t <- term
    return (f * t)
    <|> return f

factor :: Parser Int
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    return e
    <|> natural

eval :: String -> Int
eval xs =
  case parse expr xs of
    [(n, [])] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"

--- Calculator

box :: [String]
box =
  [ "+---------------+",
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
    "+---+---+---+---+"
  ]

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=1234567890+-*()/"
    extra = "QCD \ESC\BS\DEL\n"

writeAt :: (Int, Int) -> String -> IO ()
writeAt p xs = do
  goto p
  putStr xs

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

cls :: IO ()
cls = putStr "\ESC[2J"

beep :: IO ()
beep = putStr "\BEL"

showbox :: IO ()
showbox = sequence_ [writeAt (1, y) b | (y, b) <- zip [1 ..] box]

display :: [Char] -> IO ()
display xs = do
  writeAt (3, 2) (replicate 13 ' ')
  writeAt (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do
  display xs
  c <- getCh
  if elem c buttons
    then do
      process c xs
    else do
      beep
      calc xs

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC" = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n" = evalCalc xs
  | elem c "cC" = clear
  | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc $ init xs

evalCalc :: String -> IO ()
evalCalc xs =
  case parse expr xs of
    [(n, [])] -> calc (show n)
    _ -> do
      beep
      calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
  cls
  showbox
  clear

-- 1.

comment :: Parser ()
comment = do
  string "--"
  many $ sat (/= '\n')
  return ()

-- 4.

data Factor
  = Parens Expr
  | Val Int
  deriving (Show)

data UnaryOp
  = Expo Factor Factor
  | Factor Factor
  deriving (Show)

data Term
  = SimpleTerm UnaryOp
  | MulTerm UnaryOp Term
  | DivTerm UnaryOp Term
  deriving (Show)

data Expr
  = SimpleExpr Term
  | AddExpr Term Expr
  | SubExpr Term Expr
  deriving (Show)

-- 2 + 3
e1 :: Expr
e1 =
  AddExpr
    (SimpleTerm (Factor $ Val 2))
    (SimpleExpr $ SimpleTerm (Factor $ Val 3))

parensParser :: Parser Factor
parensParser =
  do
    symbol "("
    e <- exprParser
    symbol ")"
    return $ Parens e

valParser :: Parser Factor
valParser = do
  n <- int
  return $ Val n

factorParser :: Parser Factor
factorParser = parensParser <|> valParser

parserUnaryOp :: Parser UnaryOp
parserUnaryOp =
  do
    base <- factorParser
    symbol "^"
    exp <- factorParser
    return $ Expo base exp
    <|> do
      f <- factorParser
      return $ Factor f

termParser :: Parser Term
termParser =
  do
    f <- parserUnaryOp
    do
      symbol "*"
      t <- termParser
      return $ MulTerm f t
      <|> do
        symbol "/"
        t <- termParser
        return $ DivTerm f t
      <|> do
        return $ SimpleTerm f

exprParser :: Parser Expr
exprParser =
  do
    t <- termParser
    do
      symbol "+"
      e <- exprParser
      return $ AddExpr t e
      <|> do
        symbol "-"
        e <- exprParser
        return $ SubExpr t e
      <|> do
        return $ SimpleExpr t

evalFactor :: Factor -> Int
evalFactor (Parens e) = evalExpr e
evalFactor (Val n) = n

evalUnaryOp :: UnaryOp -> Int
evalUnaryOp (Expo base exp) = evalFactor base ^ evalFactor exp
evalUnaryOp (Factor f) = evalFactor f

evalTerm :: Term -> Int
evalTerm (SimpleTerm f) = evalUnaryOp f
evalTerm (MulTerm f t) = evalUnaryOp f * evalTerm t
evalTerm (DivTerm f t) = evalUnaryOp f `div` evalTerm t

evalExpr :: Expr -> Int
evalExpr (SimpleExpr t) = evalTerm t
evalExpr (AddExpr t e) = evalTerm t + evalExpr e
evalExpr (SubExpr t e) = evalTerm t - evalExpr e

getExpr :: String -> Maybe Expr
getExpr s =
  case parse exprParser s of
    [] -> Nothing
    [(e, _)] -> Just e