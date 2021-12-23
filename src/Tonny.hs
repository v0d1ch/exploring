{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Tonny where

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry f l =
    case l of
      []     -> []
      (a:as) -> f a : furry f as

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f m =
    case m of
      Nothing -> Nothing
      Just a  -> Just (f a)

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f f' = (\a -> f (f' a))

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft e) =
    case e of
      Left a  -> EitherLeft (Left $ f a)
      Right b -> EitherLeft (Right b)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight e)=
    case e of
      Left a  -> EitherRight (Left a)
      Right b -> EitherRight (Right $ f b)

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f ma = banana (\a -> unicorn $ f a) ma

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana :: (a -> [b]) -> [a] -> [b]
  banana f l =
    case l of
      []     -> []
      (a:as) -> f a <> banana f as

  unicorn :: a -> [a]
  unicorn a = [a]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana :: (a -> Maybe b) -> Maybe a -> Maybe b
  banana f ma =
    case ma of
      Nothing -> Nothing
      Just a  -> f a
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f fa = (\a -> let t = fa a in f t a)
  unicorn a = \_ -> a

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft el) =
    case el of
      Left a -> f a
      Right b -> (EitherLeft $ Right b)
  unicorn a = EitherLeft (Left a)

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana f (EitherRight el) =
    case el of
      Left a -> (EitherRight $ Left a)
      Right b -> f b
  unicorn a = EitherRight (Right a)

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma f = banana (\ab -> furry' ab ma) f

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy as f =
  foldr (\a mbs ->
           banana
             (\b ->
                 (banana (\bs -> unicorn (b : bs)) mbs)
             ) (f a)
        ) (unicorn []) as

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage as = moppy as id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = apple mb (furry' f ma)
  -- apple mb (apple ma (unicorn f))
  -- jellybean $ furry' (\a -> furry' (\b -> f a b ) mb ) ma

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = apple mc (banana2 f ma mb)
  -- jellybean $ furry' (\f' -> banana2 f' mb mc) (apple ma (unicorn f))

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = apple md (banana3 f ma mb mc)

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  -- furry :: (a -> b) -> State s a -> State s b
  furry f sf =
    State (\s ->
               let (s', a) = state sf s
               in (s', f a)
            )

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana :: (a -> State s b) -> State s a -> State s b
  banana f st =
    State
      (\s ->
         let (s', a) = state st s
         in state (f a) s'
      )
  unicorn :: a -> State s a
  unicorn a = State (\s -> (s, a))
