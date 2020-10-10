module Main where

import Data.Char

main :: IO ()
main = putStrLn "Hello, Haskell! Welcome"

prod :: Num p => [p] -> p
prod [] = 1
prod (x : xs) = x * prod xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

halve :: [a] -> ([a], [a])
halve xs = (take l xs, drop l xs)
  where
    l = length xs `div` 2

third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_ : _ : z : _) = z

safeTail1 :: [a] -> [a]
safeTail1 xs =
  if null xs
    then xs
    else tail xs

safeTail2 :: [a] -> [a]
safeTail2 xs
  | null xs = xs
  | otherwise = tail xs

safeTail3 :: [a] -> [a]
safeTail3 [] = []
safeTail3 xs = tail xs

mult = \x -> \y -> \z -> x * y * z

-- 8.

subNine :: Int -> Int
subNine x
  | x > 9 = x - 9
  | otherwise = x

luhnDouble :: Int -> Int
luhnDouble x = subNine d
  where
    d = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod (a2 + subNine b + c2 + subNine d) 10 == 0
  where
    c2 = luhnDouble c
    a2 = luhnDouble a

-- Chapter 5 --

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [y | y <- xs, y == x]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where
    n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

crack :: String -> String
crack xs = encode (- factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table2) table | n <- [0 .. 25]]
    table2 = freqs xs

xs = "kdvnhoo lv ixq"

table :: [Float]
table =
  [ 8.1,
    1.5,
    2.8,
    4.2,
    12.7,
    2.2,
    2.0,
    6.1,
    7.0,
    0.2,
    0.8,
    4.0,
    2.4,
    6.7,
    7.5,
    1.9,
    0.1,
    6.0,
    6.3,
    9.0,
    2.8,
    1.0,
    2.4,
    0.2,
    2.0,
    0.1
  ]

-- 1.

sumFirst100Sq = sum [x * x | x <- [1 .. 100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4.

replicate2 :: Int -> a -> [a]
replicate2 k x = [x | _ <- [1 .. k]]

-- 5.

pyths :: Int -> [(Int, Int, Int)]
pyths k = [(x, y, z) | x <- [1 .. k], y <- [1 .. k], z <- [1 .. k], x ^ 2 + y ^ 2 == z ^ 2]

-- 6.

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects m = [x | x <- [1 .. m], sum (factors x) - x * 2 == 0]

-- 7.

c1 = [(x, y) | x <- [1, 2], y <- [3, 4]]

-- c2 = [(x, y) | x <- [1, 2]]