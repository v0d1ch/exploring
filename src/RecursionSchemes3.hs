{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}

module RecursionSchemes3 where

import Control.Monad.Fix
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data Test =
  Test
    { testA :: String
    , testB :: Int
    } deriving Show

makeBaseFunctor ''Test

-- data TestF r = TestF {testAF :: [Char], testBF :: Int}

testL :: [Test]
testL = [Test "a" 1, Test "b" 2, Test "c" 3, Test "d" 4, Test "e" 5, Test "c" 7]

foldTest :: [Test] -> Int
foldTest = cata alg
  where
    alg :: ListF Test Int -> Int
    alg Nil = 0
    alg (Cons a ints) = testB a + ints

foldTestS :: [Test] -> String
foldTestS = cata alg
  where
    alg :: ListF Test String -> String
    alg Nil = ""
    alg (Cons a ints) = testA a <> ints

f :: Test -> Bool
f a = testA a == "c"

filterF :: forall a . (a -> Bool) -> [a] -> [a]
filterF filterBy = cata alg
  where
    alg :: ListF a [a] -> [a]
    alg  Nil = []
    alg (Cons a as) = if filterBy a then a : as else as
