module Main where

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