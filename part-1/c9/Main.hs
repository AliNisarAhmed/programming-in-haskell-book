module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Expo

instance Show Op where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "
  show Expo = " ^ "

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
valid Expo _ y = y > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Expo x y = x ^ y

data Expr
  = Val Int
  | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid2 o x y]

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = yss ++ map (x :) yss
  where
    yss = subsets xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subsets

choicesCompre :: [a] -> [[a]]
choicesCompre xs = [y | s <- subsets xs, y <- perms s]

solution :: Expr -> [Int] -> Int -> Bool
solution exp numbers target =
  elem (values exp) (choices numbers) && eval exp == [target]

-- Brute Force Solution ----

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Expo]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

---- Combining generation and evaluation to improve performance

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns target =
  [e | ns' <- choices ns, (e, m) <- results ns', m == target]

---- Using commutative property to further improve performance

-- commutative variants of an expression are considered invalid, so that only one expression is allowed

valid2 :: Op -> Int -> Int -> Bool
valid2 Add x y = x <= y
valid2 Sub x y = x > y
valid2 Mul x y = x > 1 && y > 1 && x <= y
valid2 Div x y = y > 1 && x `mod` y == 0
valid2 Expo _ y = y > 1

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n, n) | n > 0]
results' ns = [res | (ls, rs) <- split ns, lx <- results' ls, ry <- results' rs, res <- combine'' lx ry]

combine'' :: Result -> Result -> [Result]
combine'' (l, x) (r, y) =
  [(App o l r, apply o x y) | o <- ops, valid2 o x y]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns target =
  [e | ns' <- choices ns, (e, m) <- results ns', m == target]

-- 2.

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst n (x : xs)
  | n == x = xs
  | otherwise = x : removeFirst n xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x : xs) ys = removed /= ys && isChoice xs (removeFirst x ys)
  where
    removed = removeFirst x ys

-- 4.

list :: [Int]
list = [1, 3, 7, 10, 25, 50]

allChoices :: [[Int]]
allChoices = choices list

allExprs = concatMap exprs allChoices

validEval = length . filter (/= []) . map eval $ allExprs

-- We allow all integers as long as they dont result in 0, coz we dont want division by zero
valid3 :: Op -> Int -> Int -> Bool
valid3 Add _ _ = True
valid3 Sub _ _ = True
valid3 Mul _ _ = True
valid3 Div x y = y /= 0 && x `mod` y == 0
valid3 Expo _ y = y > 1