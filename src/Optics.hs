{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Optics where

-- import           Control.Applicative
import           Control.Lens
-- import           Data.Char
-- import qualified Data.Map            as M
-- import qualified Data.Set            as S
-- import qualified Data.Text           as T

data Ship =
    Ship { _name    :: String
         , _numCrew :: Int
         } deriving (Show)


lensShipName :: Lens' Ship String
lensShipName = lens _name setShipName

setShipName :: Ship -> String -> Ship
setShipName ship s = ship { _name = s }

data Pet = Pet
    { _petName :: String
    , _petType :: String
    }

makeLenses ''Pet

getPetName :: Pet -> String
getPetName pet = view petName pet

--Write the type signature of the polymorphic lens which would allow changing
-- a Vorpal x to a Vorpal y.
-- x  :: Lens (Vorpal x) (Vorbal y) x y
data Preferences a =
    Preferences
      { _best  :: a
      , _worst :: a
      } deriving (Show)

prefLens :: Lens (Preferences a) (Preferences b) (a,a) (b,b)
prefLens = lens getter setter
  where
    getter :: Preferences a -> (a, a)
    getter Preferences {..}= (_best, _worst)
    setter :: Preferences a -> (b,b) -> Preferences b
    setter pref (val1, val2) = pref {_best = val1, _worst = val2}

data Result e =
    Result { _lineNumber :: Int
           , _result     :: Either e String
           }

resultLens :: Lens (Result a) (Result b) (Either a String) (Either b String)
resultLens = undefined

data Predicate a =
    Predicate (a -> Bool)

predicateLens :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicateLens = lens getter setter
  where
    getter :: Predicate a -> (a -> Bool)
    getter (Predicate f) = f
    setter :: Predicate a -> (b -> Bool) -> Predicate b
    setter _ f = Predicate f
