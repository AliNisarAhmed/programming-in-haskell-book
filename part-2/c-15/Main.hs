module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- 4.

fibs :: [Integer]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

-- 5.

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show)

repeat' :: Tree a -> [Tree a]
repeat' t = ts
  where
    ts = t : ts

take' :: Int -> [Tree a] -> [Tree a]
take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

replicate' :: Int -> Tree a -> [Tree a]
replicate' n = take' n . repeat'

-- 6.

distance :: Double
distance = 0.00001

next :: Fractional a => a -> a -> a
next n a = (a + n / a) / 2

sqroot :: Double -> Double
sqroot n = head $ [m | (m, k) <- zip guesses (tail guesses), abs (m - k) < distance]
  where
    guesses = iterate (next n) 1.0