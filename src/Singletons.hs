{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Singletons where

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.TH

data Foo a = MKFoo

data DoorState
  = Opened
  | Closed
  | Locked
  deriving (Show, Eq)

data Door  :: DoorState -> Type where
  UnsafeMkDoor :: { doorMaterial :: String } -> Door s

deriving instance Show (Door a)

genSingletons [''DoorState]

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

mkDoor :: DoorState -> String -> Door s
mkDoor Opened = UnsafeMkDoor
mkDoor Closed = UnsafeMkDoor
mkDoor Locked = UnsafeMkDoor

withSingDSI :: SingI s => SDoorState s -> r -> r
withSingDSI sng x = case sng of
  SOpened -> x
  SClosed -> x
  SLocked -> x
