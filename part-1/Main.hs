module Main where

import Data.Char
import Data.List hiding (find)

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

ulet2int :: Char -> Int
ulet2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2ulet :: Int -> Char
int2ulet n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2ulet ((ulet2int c + n) `mod` 26)
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
    table2 = freqs (map toLower xs)

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

c2 :: [(Int, Int)]
c2 = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

-- 8.

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [0 ..])

-- 9.

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- Chapter 6

-- 2.

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3.

(^%) :: Int -> Int -> Int
(^%) b 1 = b
(^%) b e = b * (b ^% (e - 1))

-- 4.

euclid :: Int -> Int -> Int
euclid a b
  | a == b = a
  | a > b = euclid (a - b) b
  | otherwise = euclid a (b - a)

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (False : _) = False
myAnd (_ : xs) = myAnd xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs

replicate3 :: Int -> a -> [a]
replicate3 0 _ = []
replicate3 n xs = xs : replicate3 (n - 1) xs

selectNth :: [a] -> Int -> a
selectNth xs m | m >= length xs = error "Index out of range"
selectNth (x : _) 0 = x
selectNth (_ : xs) m = selectNth xs (m - 1)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem k (x : xs)
  | k == x = True
  | otherwise = myElem k xs

-- 7.

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- 8.

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort first) (msort second)
  where
    (first, second) = halve xs

-- 9.

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' m (x : xs) = x : take' (m - 1) xs

selectLast :: [a] -> a
selectLast [x] = x
selectLast (_ : xs) = selectLast xs

type Bit = Int

bin2int2 :: [Bit] -> Int
bin2int2 bits = sum [w * v | (w, v) <- zip bits weights]
  where
    weights = iterate (* 2) 1

bin2int :: [Bit] -> Int
bin2int = foldr (\x acc -> x + 2 * acc) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encodeToBit :: String -> [Bit]
encodeToBit = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decodeBits :: [Bit] -> String
decodeBits = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decodeBits . channel2 . encodeToBit

channel :: [Bit] -> [Bit]
channel = id

-- 7.

channel2 :: [Bit] -> [Bit]
channel2 = checkParity . faultyMedium . addParity

faultyMedium :: [[Bit]] -> [[Bit]]
faultyMedium = map tail

checkParity :: [[Bit]] -> [Bit]
checkParity signal = concatMap f signal
  where
    f [] = []
    f (x : xs)
      | even countOnes && x == 0 = xs
      | odd countOnes && x == 1 = xs
      | otherwise = error "Parity check failed"
      where
        countOnes = countX 1 xs

addParity :: [Bit] -> [[Bit]]
addParity = map f . chop8
  where
    f :: [Bit] -> [Bit]
    f bs = parity : bs
      where
        parity
          | even $ countX 1 bs = 0
          | otherwise = 1

bits :: [Bit]
bits = [1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0]

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

countX :: Eq a => a -> [a] -> Int
countX x = length . filter (== x)

removeDup :: Eq a => [a] -> [a]
removeDup [] = []
removeDup (x : xs) = x : filter (/= x) (removeDup xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(countX v vs, v) | v <- removeDup vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

----

ballots :: [[String]]
ballots =
  [ ["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]
  ]

removeEmpty :: Eq a => [[a]] -> [[a]]
removeEmpty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner2 :: Ord a => [[a]] -> a
winner2 bs =
  case rank (removeEmpty bs) of
    [c] -> c
    (c : cs) -> winner2 (elim c bs)

lc1 :: (t -> a) -> (t -> Bool) -> [t] -> [a]
lc1 f p xs = [f x | x <- xs, p x]

lc1' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lc1' f p = map f . filter p

---
-- 2.

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = foldr (\x acc -> if not $ f x then False else acc) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> if f x then True else acc) False

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f =
  snd
    . foldl
      ( \acc x ->
          if fst acc
            then if f x then (True, snd acc ++ [x]) else (False, snd acc)
            else acc
      )
      (True, [])

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f =
  snd
    . foldl
      ( \acc x ->
          if fst acc
            then if f x then (True, snd acc) else (False, snd acc ++ [x])
            else (True, snd acc ++ [x])
      )
      (True, [])

takeWhile2 _ [] = []
takeWhile2 p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

dropWhile2 _ [] = []
dropWhile2 p (x : xs)
  | p x = dropWhile2 p xs
  | otherwise = xs

map2 :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map2 f = foldr (\x acc -> f x : acc) []

filter2 p = foldr (\x acc -> if p x then x : acc else acc) []

dec2Int :: [Int] -> Int
dec2Int xs = foldl (\acc x -> 10 * acc + x) 0 xs

curry2 :: ((a, b) -> c) -> (a -> b -> c)
curry2 f = \a -> \b -> f (a, b)

uncurry2 :: (a -> b -> c) -> ((a, b) -> c)
uncurry2 f = \(a, b) -> f a b

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

chop82 :: [Bit] -> [[Bit]]
chop82 = unfold (== []) (take 8) (drop 8)

mapUnfold :: (a -> b) -> [a] -> [b]
mapUnfold f = unfold null (f . head) tail

iterateUnfold :: (a -> a) -> a -> [a]
iterateUnfold f = unfold (const False) f f

-- 9.

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ [x] = [f x]
altMap f g (x : y : xs) = f x : g y : altMap f g xs

------ Chapter 8 -------

type Pos = (Int, Int)

type Trans = Pos -> Pos

type Pair a = (a, a)

type Assoc k v = [(k, v)]

findAssoc :: Eq k => k -> Assoc k v -> v
findAssoc k t = head [v | (k', v) <- t, k == k']

data Move = North | South | East | West deriving (Eq, Show)

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m : ms) p = moves ms (move m p)

reverseMov :: Move -> Move
reverseMov North = South
reverseMov South = North
reverseMov West = East
reverseMov East = West

data Shape
  = Circle Float
  | Rect Float Float
  deriving (Eq, Show)

mkSquare :: Float -> Shape
mkSquare n = Rect n n

calcArea :: Shape -> Float
calcArea (Circle r) = pi * r ^ 2
calcArea (Rect h w) = h * w

data Nat = Zero | Succ Nat deriving (Eq, Show)

addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat (Succ m) n = Succ (addNat m n)

-- Tautology Checker

data Proposition
  = Const Bool
  | Var Char
  | Not Proposition
  | And Proposition Proposition
  | Or Proposition Proposition
  | Imply Proposition Proposition
  | Equivalence Proposition Proposition

p1 :: Proposition
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Proposition
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Proposition
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Proposition
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Proposition
p5 = Imply (And (Var 'A') (Or (Var 'A') (Var 'B'))) (Var 'A')

p6 :: Proposition
p6 = Imply (Or (Var 'A') (Var 'B')) (Var 'A')

p7 :: Proposition
p7 = Equivalence (Not (Imply (Var 'A') (Var 'B'))) (And (Var 'A') (Not (Var 'B')))

p8 :: Proposition
p8 = Imply (And (Var 'A') (Var 'B')) (Or (Var 'A') (Var 'B'))

type SubsTable = Assoc Char Bool

eval :: SubsTable -> Proposition -> Bool
eval _ (Const b) = b
eval s (Var x) = findAssoc x s
eval s (Not p) = not (eval s p)
eval s (Or p1 p2) = eval s p1 || eval s p2
eval s (And p1 p2) = eval s p1 && eval s p2
eval s (Imply p q) = eval s p <= eval s q
eval s (Equivalence p q) = eval s p == eval s q

vars :: Proposition -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equivalence p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where
    bss = bools (n - 1)

getSubsTable :: Proposition -> [SubsTable]
getSubsTable p = map (zip vs) (bools (length vs))
  where
    vs = removeDup $ vars p

isTaut :: Proposition -> Bool
isTaut p = and [eval s p | s <- getSubsTable p]

---- Abstract Machine ----

data Expr
  = Val Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show)

value :: Expr -> Int
value (Val x) = x
value (Add e1 e2) = value e1 + value e2
value (Mul e1 e2) = value e1 * value e2

type ControlStack = [Operations]

data Operations
  = ADD Expr
  | MUL Expr
  | PLUS Int
  | TIMES Int

evalE :: Expr -> ControlStack -> Int
evalE (Val n) c = exec c n
evalE (Add e1 e2) c = evalE e1 (ADD e2 : c)
evalE (Mul e1 e2) c = evalE e1 (MUL e2 : c)

exec :: ControlStack -> Int -> Int
exec [] n = n
exec (ADD y : c) n = evalE y (PLUS n : c)
exec (MUL y : c) n = evalE y (TIMES n : c)
exec (PLUS n : c) m = exec c (n + m)
exec (TIMES n : c) m = exec c (n * m)

valueE :: Expr -> Int
valueE e = evalE e []

-- 1.

-- addNat :: Nat -> Nat -> Nat
-- addNat Zero n = n
-- addNat (Succ m) n = Succ (addNat m n)

mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat (Succ m) n = addNat n (mulNat m n)

three :: Nat
three = Succ (Succ (Succ Zero))

four :: Nat
four = Succ three

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Succ n) = 1 + nat2Int n

-- 2.

data Tree2 a
  = Leaf2 a
  | Node2 (Tree2 a) a (Tree2 a)
  deriving (Eq, Show)

occurs :: Ord a => a -> Tree2 a -> Bool
occurs x (Leaf2 y) = x == y
occurs x (Node2 l v r) =
  case compare x v of
    EQ -> True
    LT -> occurs x l
    _ -> occurs x r

-- 3.

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

numberOfLeafs :: Tree a -> Int
numberOfLeafs (Leaf _) = 1
numberOfLeafs (Node l r) = numberOfLeafs l + numberOfLeafs r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node t1 t2) = numberOfLeafs t1 - numberOfLeafs t2 <= 1 && balanced t1 && balanced t2

-- 4.

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance left) (balance right)
  where
    (left, right) = halve xs

-- 5.

-- data Expr
--   = Val Int
--   | Add Expr Expr
--   deriving (Eq, Show)

folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
folde f _ _ (Val n) = f n
folde f g h (Add e1 e2) = g (folde f g h e1) (folde f g h e2)
folde f g h (Mul e1 e2) = h (folde f g h e1) (folde f g h e2)

-- 6.

eval2 :: Expr -> Int
eval2 = folde id (+) (*)

e1 :: Expr
e1 = Add (Add (Val 5) (Val 6)) (Add (Val 7) (Add (Val 8) (Val 9)))

e2 :: Expr
e2 =
  Mul
    (Add (Val 2) (Val 3))
    (Add (Val 4) (Val 5))

size :: Expr -> Int
size = folde (const 1) (+) (+)

-- 7.

-- instance Eq a => Eq (Maybe a) where
--   Nothing == Nothing = True
--   Just x == Just y = x == y
--   _ == _ = False

-- instance Eq a => Eq [a] where
--   (==) (x : xs) (y : ys) = x == y && xs == ys
--   (==) [] [] = True
