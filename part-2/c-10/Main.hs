module Main where

import Data.Char
import System.IO
import Text.Read
import Prelude

main :: IO ()
main = undefined

act :: IO (Char, Char)
act = do
  x <- getChar
  getChar
  y <- getChar
  return (x, y)

-- getLine :: IO String
-- getLine = do
--   x <- getChar
--   if x == '\n'
--     then return []
--     else do
--       xs <- getLine
--       return (x : xs)

-- putStr :: String -> IO ()
-- putStr [] = return ()
-- putStr (x : xs) = do
--   putChar x
--   putStr xs

-- putStrLn :: String -> IO ()
-- putStrLn xs = do
--   putStr xs
--   putChar '\n'

strlen :: IO ()
strlen = do
  putStr "Enter a string: "
  str <- getLine
  putStr "The string has "
  putStr (show $ length str)
  putStrLn " characters"

-- Hangman --

hangman :: IO ()
hangman = do
  putStrLn "Think of a word: "
  word <- sgetLine
  putStrLn "Try to guess it:"
  play word

sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n'
    then (putChar x >>= (\_ -> return ""))
    else do
      putChar '-'
      xs <- sgetLine
      return (x : xs)

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word
    then putStrLn "You got it!!"
    else do
      putStrLn (match word guess)
      play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

-- Nim

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num =
  [update r n | (r, n) <- zip [1 ..] board]
  where
    update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row numStars = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate numStars "* "))

putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do
  putRow 1 a
  putRow 2 b
  putRow 3 c
  putRow 4 d
  putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do
  putStrLn prompt
  x <- getChar
  newLine
  if isDigit x
    then return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt

newLine :: IO ()
newLine = putChar '\n'

playNim :: Board -> Int -> IO ()
playNim board player = do
  newLine
  putBoard board
  if finished board
    then do
      newLine
      putStr "Player "
      putStr (show $ next player)
      putStr " wins!!"
    else do
      newLine
      putStr "Player "
      putStrLn (show player)
      row <- getDigit "Enter a row number: "
      num <- getDigit "Stars to remove: "
      if valid board row num
        then playNim (move board row num) (next player)
        else do
          newLine
          putStrLn "ERROR: Invalid move"
          playNim board player

nim :: IO ()
nim = playNim initial 1

-- Game of Life

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeAt :: Pos -> String -> IO ()
writeAt p xs = do
  goto p
  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type GameBoard = [Pos]

glider :: GameBoard
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showCells :: GameBoard -> IO ()
showCells b = sequence_ [writeAt p "0" | p <- b]

isAlive :: GameBoard -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: GameBoard -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) =
  map
    wrap
    [ (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      (x + y, y),
      (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1)
    ]

wrap :: Pos -> Pos
wrap (x, y) = ((x - 1) `mod` width + 1, (y - 1) `mod` height + 1)

liveNeighbs :: GameBoard -> Pos -> Int
liveNeighbs b = length . filter (isAlive b) . neighbs

survivors :: GameBoard -> [Pos]
survivors b = [p | p <- b, elem (liveNeighbs b p) [2, 3]]

births :: GameBoard -> [Pos]
births b =
  [ p | p <- rmdups (concat (map neighbs b)), isEmpty b p, liveNeighbs b p == 3
  ]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

nextgen :: GameBoard -> GameBoard
nextgen b = survivors b ++ births b

life :: GameBoard -> IO ()
life b = do
  cls
  showCells b
  wait 500000
  life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1 .. n]]

----

-- 1.

putStrLn2 :: String -> IO ()
putStrLn2 xs = sequence_ [putChar x | x <- (xs ++ ['\n'])]

-- 2.

-- putBoard :: Board -> IO ()
-- putBoard [a, b, c, d, e] = do
--   putRow 1 a
--   putRow 2 b
--   putRow 3 c
--   putRow 4 d
--   putRow 5 e

putBoard2 :: Int -> Board -> IO ()
putBoard2 _ [] = pure ()
putBoard2 r (x : xs) = do
  putRow r x
  putBoard2 (r + 1) xs

putBoard3 :: Int -> Board -> IO ()
putBoard3 r xs = sequence_ [putRow m x | x <- xs, m <- [1 .. r]]

-- 4.

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  putChar ' '
  text <- getLine
  case readMaybe text :: Maybe Int of
    Nothing -> invalid
    Just n -> if n > 0 then return n else invalid
  where
    invalid = do
      putStrLn "Invalid natural Number!"
      getNat prompt

getInt :: IO Int
getInt = do
  text <- getLine
  case readMaybe text :: Maybe Int of
    Nothing -> do
      putStrLn "Invalid integer!"
      getInt
    Just n -> return n

adder :: IO ()
adder = do
  n <- getNat "How many numbers?"
  ints <- sequence (replicate n getInt)
  putStr ("The total is " ++ show (sum ints) ++ "\n")

readLine :: IO String
readLine = do
  c <- getCh
  case c of
    '\n' -> do
      putChar '\n'
      return []
    '\DEL' -> do
      putStr "\b \b"
      cs <- readLine
      return ('\DEL' : cs)
    otherwise -> do
      putChar c
      cs <- readLine
      return
        ( case cs of
            [] -> [c]
            ('\DEL' : cs') -> cs'
            otherwise -> c : cs
        )