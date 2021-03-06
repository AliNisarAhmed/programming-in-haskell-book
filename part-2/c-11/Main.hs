module Main where

import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import System.IO
import System.Random (randomRIO)

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player -> Player
turn g p =
  if os < xs
    then O
    else
      if os > xs
        then X
        else next p
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any samePlayer (rows ++ cols ++ diags)
  where
    samePlayer = all (== p)
    rows = g
    cols = transpose g
    diags = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

won :: Grid -> Bool
won g = wins O g || wins X g

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

bar1 = [replicate ((size * 4) -1) '-']

bar2 = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

valid :: Grid -> Int -> Bool
valid g i = i >= 0 && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i
    then [chop size (xs ++ [p] ++ ys)]
    else []
  where
    (xs, B : ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return $ read xs
    else do
      putStrLn "ERROR: Invalid Number"
      getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

--------

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-----

g1 = concat $ move empty 0 X

g2 = concat $ move g1 1 O

g3 = concat $ move g2 2 X

g4 = concat $ move g3 3 O

g5 = concat $ move g4 4 X

g6 = concat $ move g5 5 O

g7 = concat $ move g6 6 O

g8 = concat $ move g7 7 X

g9 = concat $ move g8 8 O

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins! \n"
  | wins X g = putStrLn "Player X wins! \n"
  | full g = putStrLn "It's a draw! \n"
  | otherwise = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        run' g p
      [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a
  = Node a [Tree a]
  deriving (Show)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. ((size ^ 2) - 1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Player -> Tree (Grid, Player)
minimax (Node g []) player
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts) player
  | turn g player == O = Node (g, minimum ps) ts'
  | turn g player == X = Node (g, maximum ps) ts'
  where
    ts' = map (flip minimax $ player) ts
    ps = [p | Node (_, p) _ <- ts']

-- minimax :: Tree Grid -> Player -> Tree (Grid, Player)
-- minimax (Node g []) player
--   | wins O g = Node (g, O) []
--   | wins X g = Node (g, X) []
--   | otherwise = Node (g, B) []
-- minimax (Node g ts) player
--   | turn g player == O = Node (g, fromMaybe B $ find (== O) ps) ts'
--   | turn g player == X = Node (g, fromMaybe B $ find (== X) ps) ts'
--   where
--     ts' = map (flip minimax $ player) ts
--     ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Tree Grid -> Grid
bestmove g p tree =
  getGridFromLabeledTree $ minimumBy depthCompare $ [t | t <- ts, getPlayerFromLabeledTree t == best]
  where
    Node (_, best) ts = minimax tree p

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Do you want to make the first move? (y/n):"
  res <- getLine
  case res of
    "y" -> play empty O emptyGridTree
    "n" -> play computerFirstMove O computerGameTree
    _ -> do
      putStrLn "Please enter either y or n: "
      main
  where
    computerFirstMove = concat $ move empty 4 X
    emptyGridTree = gametree empty O
    computerGameTree = gametree computerFirstMove X

play :: Grid -> Player -> Tree Grid -> IO ()
play g p gt =
  do
    cls
    goto (1, 1)
    putGrid g
    play' g p gt

play' :: Grid -> Player -> Tree Grid -> IO ()
play' g p gt
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        play' g p gt
      [g'] ->
        play g' (next p) (nextGameTree gt g')
  | p == X =
    do
      putStr "Player X is thinking... "
      let nextmove = bestmove g p gt
          nextTree = nextGameTree gt nextmove
      (play $! nextmove) (next p) nextTree

-- 1.

countNodes :: Tree Grid -> Int
countNodes (Node _ []) = 1
countNodes (Node _ subtree) = 1 + (sum $ map countNodes subtree)

-- 3.

getPlayerFromLabeledTree :: Tree (Grid, Player) -> Player
getPlayerFromLabeledTree (Node (_, p) _) = p

getGridFromLabeledTree :: Tree (Grid, Player) -> Grid
getGridFromLabeledTree (Node (g, _) _) = g

calcDepth :: Tree a -> Int
calcDepth (Node _ []) = 0
calcDepth (Node _ ts) = 1 + (maximum $ map calcDepth ts)

depthCompare :: Tree a -> Tree a -> Ordering
depthCompare n1 n2 = compare (calcDepth n1) (calcDepth n2)

-- 4.

nextGameTree :: Tree Grid -> Grid -> Tree Grid
nextGameTree x@(Node _ ts) g =
  case bestmoveGrid of
    Nothing -> x
    Just v -> v
  where
    bestmoveGrid = safeHead $ filter (\(Node v _) -> v == g) ts

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x