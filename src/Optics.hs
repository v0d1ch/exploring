{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Optics where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

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
