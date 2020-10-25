module Main where

import Data.Char

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S sf) s = sf s

instance Functor ST where
  fmap f st =
    S
      ( \s ->
          let (x, s') = app st s
           in (f x, s')
      )

instance Applicative ST where
  pure a = S (\s -> (a, s))
  sf <*> sx =
    S
      ( \s ->
          let (f, s') = app sf s
              (x, s'') = app sx s'
           in (f x, s'')
      )

instance Monad ST where
  return = pure
  sa >>= sf =
    S
      ( \s ->
          let (a, s') = app sa s
           in app (sf a) s'
      )

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

freshInt :: ST Int
freshInt = S (\n -> (n, n + 1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> freshInt
alabel (Node l r) = Node <$> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do
  n <- freshInt
  return (Leaf n)
mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return $ Node l' r'

-- 1.

data Tree2 a
  = Leaf2
  | Node2 (Tree2 a) a (Tree2 a)
  deriving (Show)

instance Functor Tree2 where
  fmap _ Leaf2 = Leaf2
  fmap f (Node2 l v r) = Node2 (fmap f l) (f v) (fmap f r)

-- 2.

newtype Func r a = Func {getFunc :: r -> a}

instance Functor (Func r) where
  fmap f (Func ra) =
    Func $ f . ra

instance Applicative (Func r) where
  pure a = Func $ const a
  (Func rab) <*> (Func ra) = Func (rab <*> ra)

-- <*> can also be defined like this
-- (Func rab) <*> (Func ra) = Func (\r -> (rab r) (ra r))
-- pure for Func resembles K Combinator where K x y = x
-- <*> for Func resembles S combinator where S x y z = (x z) (y z)

-- 4.

newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
  fmap f (Z xs) = Z (fmap f xs)

instance Applicative ZipList where
  pure x = Z (repeat x)
  (Z fs) <*> (Z xs) = Z [g x | (g, x) <- zip fs xs]

-- 5.

instance Monad (Func r) where
  return = pure
  (Func ra) >>= f =
    Func
      ( \r -> (getFunc $ f (ra r)) r
      )

-- 7.

data Expr a
  = Var a
  | Val Int
  | Add (Expr a) (Expr a)
  deriving (Show)

instance Functor Expr where
  fmap f (Var a) = Var $ f a
  fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)
  fmap _ (Val x) = Val x

instance Applicative Expr where
  pure = Var
  (Var f) <*> (Var a) = Var $ f a
  (Var f) <*> (Add x1 x2) = Add (pure f <*> x1) (pure f <*> x2)
  (Add f1 _) <*> (Var a) = (f1 <*> (pure a))
  (Add f1 f2) <*> Add x1 x2 = Add (f1 <*> x1) (f2 <*> x2)
  _ <*> Val x = Val x
  Val x <*> _ = Val x

instance Monad Expr where
  return = pure
  (Val n) >>= _ = Val n
  (Var a) >>= f = f a
  (Add e1 e2) >>= f = do
    e1' <- e1
    e2' <- e2
    Add (f e1') (f e2')

-- checking Laws
add1 = Add (Add (Val 1) (Var "a")) (Var "b")

add2 = Var "c"

g = ord

h = toUpper