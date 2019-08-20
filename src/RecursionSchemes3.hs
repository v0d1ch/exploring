{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module RecursionSchemes3 where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data Test =
  Test
    { testA :: String
    , testB :: Int
    } deriving Show

makeBaseFunctor ''Test

testL :: [Test]
testL = [Test "a" 1, Test "b" 2, Test "c" 3, Test "d" 4, Test "e" 5, Test "c" 7]

foldTest :: [Test] -> Int
foldTest = cata alg
  where
    alg :: ListF Test Int -> Int
    alg Nil           = 0
    alg (Cons a ints) = testB a + ints

foldTestS :: [Test] -> String
foldTestS = cata alg
  where
    alg :: ListF Test String -> String
    alg Nil           = ""
    alg (Cons a ints) = testA a <> ints

f :: Test -> Bool
f a = testA a == "c"

filterF :: forall a . (a -> Bool) -> [a] -> [a]
filterF filterBy = cata alg
  where
    alg :: ListF a [a] -> [a]
    alg  Nil        = []
    alg (Cons a as) = if filterBy a then a : as else as

filterP :: forall a . Show a => (a -> Bool) -> [a] -> IO [a]
filterP filterBy = cataA alg
  where
    alg :: Base [a] (IO [a]) -> IO [a]
    alg  Nil = return []
    alg (Cons a as) =
      if filterBy a
        then do
          x <- as
          print x
          return $ a:x
        else
          as

anaT :: Int -> [Int]
anaT = ana coalg
  where
    coalg :: Int -> Base [Int] Int
    coalg n
      | n > 10 = Nil
      | otherwise = Cons n (n + 1)
