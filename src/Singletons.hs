{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Singletons where

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Base.TH

$( singletons [d|
    data DoorState
      = Opened
      | Closed
      | Locked
      deriving (Show, Eq)
   |]
 )

-- genSingletons [''DoorState]

data Door  :: DoorState -> Type where
  UnsafeMkDoor :: { doorMaterial :: String } -> Door s

deriving instance Show (Door a)


closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

closeLocked :: Door 'Locked -> Door 'Closed
closeLocked (UnsafeMkDoor m) = UnsafeMkDoor m

mkDoor :: DoorState -> String -> Door s
mkDoor Opened = UnsafeMkDoor
mkDoor Closed = UnsafeMkDoor
mkDoor Locked = UnsafeMkDoor

withSingDSI :: SDoorState s -> (SingI s => r) -> r
withSingDSI sng x = case sng of
    SOpened -> x
    SClosed -> x
    SLocked -> x


lockAnyDoor :: SDoorState s -> (Door s -> Door 'Locked)
lockAnyDoor = \case
    SOpened -> lockDoor . closeDoor  -- in this branch, s is 'Opened
    SClosed -> lockDoor              -- in this branch, s is 'Closed
    SLocked -> id

unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor x (UnsafeMkDoor s) =
  case x of
    1 -> Just $ (UnsafeMkDoor s)
    _ -> Nothing

openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor x (UnsafeMkDoor s) =
  case x of
    1 -> Just $ (UnsafeMkDoor s)
    _ -> Nothing

data OldSomeDoor :: Type where
    OldMkSomeDoor :: DoorState -> String -> OldSomeDoor

data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

instance Show SomeDoor
instance Show OldSomeDoor

toOld :: SomeDoor -> OldSomeDoor
toOld (MkSomeDoor s door) =
  OldMkSomeDoor (fromSing s) ""
