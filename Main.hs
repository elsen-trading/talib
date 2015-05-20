module Main where

import FFI

-- Sum type ( enum )
data Color = Red | Blue | Green
  deriving (Show, Ord, Eq, Read)


-- Product ( struct )
data Person = Person String Int
  deriving (Show, Ord, Read)

data T1
  = A Int
  | B String
  deriving Show

instance Eq Person where
  (Person name1 age1) == (Person name2 age2) = name1 == name2 && age1 == age2

-- Pattern matching
foo :: Color -> Int
foo x = case x of
  Red -> 1
  Blue -> 2
  Green -> 3

foo' :: Color -> Int
foo' Red = 1
foo' Blue = 2
foo' _ = 3

-- Partial functions

-- data List a = [a] | []

-- Singly-Linked list
data List a = Cons a (List a) | Nil
  deriving Show

-- Parameterized type
myList :: List String
myList = Cons "String" (Cons "foo" Nil)

data Either' a b = Left' a | Right' b
  deriving (Show)

myleft :: Either' Int a
myleft = Left' 3

myright :: Either' zfoo ()
myright = Right' ()

-- Partial functions ( parametric polymorphism )
myHead :: List a -> Either' String a
myHead (Cons a _) = Right' a
myHead Nil = Left' "You passed an empty list"

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' f [] = []
  fmap' f (x:xs) = (:) (f x) (fmap' f xs)

instance Functor' List where
  fmap' f Nil = Nil
  fmap' f (Cons a b) = Cons (f a) (fmap' f b)

instance Functor' Maybe where
  fmap' f Nothing = Nothing
  fmap' f (Just x) = Just (f x)

class Monoid' a where
  mempty :: a
  mappend :: a -> a -> a

instance Monoid' [b] where
  mempty = []
  mappend = (++)

instance Monoid' (List b) where
  mempty = Nil
  mappend = undefined

instance Monoid' () where
  mempty = ()
  mappend a b = ()

-- Typeclassopedia

-- class Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return' :: a -> m a

foobar :: IO ()
foobar = do
  a <- return 3
  print a

foobar' :: IO ()
foobar' = return 3 >>= \x -> print x

-- Monoid Laws:
-- forall a. mappend mempty a = a
-- forall a. mappend a mempty = a
-- forall a. mappend (mappend a b) c = mappend a (mappend b c)

myMap :: (a -> b) -> List a -> List b
myMap _ Nil = Nil
myMap f (Cons a rest) = Cons (f a) (myMap f rest)

myMap' :: (a -> b) -> [a] -> [b]
myMap' _ [] = []
myMap' f (x:xs) = (f x) : (myMap' f xs)

main :: IO ()
main = putStrLn "Elsen!"
