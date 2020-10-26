module Main where

import Data.Foldable

main :: IO ()
main = putStrLn "Hello, Haskell!"

newtype Func r a = Func {getFunc :: r -> a}

instance Semigroup a => Semigroup (Func r a) where
  (<>) (Func ra) (Func rb) = Func (\r -> ra r <> rb r)

instance Monoid a => Monoid (Func r a) where
  mempty = Func $ const mempty
  mappend = (<>)

data M a = J a | N deriving (Show)

instance Functor M where
  fmap f N = N
  fmap f (J x) = J $ f x

instance Foldable M where
  fold N = mempty
  fold (J x) = x

  -- foldMap :: (a -> m) -> t a -> m
  foldMap _ N = mempty
  foldMap f (J x) = f x

  -- foldr :: (a -> b -> b) -> b -> t a-> b1
  foldr _ v N = v
  foldr f v (J x) = f x v

  foldl f v N = v
  foldl f v (J x) = f v x

instance Traversable M where
  traverse _ N = pure N
  traverse f (J x) = J <$> f x

-- traverse f (J x) = sequenceA (J $ f x)

-- 4.

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Foldable Tree where
  foldMap _ Leaf = mempty
  foldMap f (Node l v r) = foldMap f l <> f v <> foldMap f r

  foldr _ v Leaf = v
  foldr f v (Node l a r) = foldr f (f a (foldr f v r)) l

  foldl _ v Leaf = v
  foldl f v (Node l a r) = foldl f (f (foldl f v l) a) r

tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- 5.

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f t = foldMap (\x -> if f x then [x] else []) t