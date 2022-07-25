{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module TypeFamilies where

import Data.Kind (Constraint)
import GHC.TypeLits

type family JustDont a :: Constraint where
  JustDont Int = ()
  JustDont String = TypeError ('Text "oops")

test :: (JustDont a, Show a) => a -> a
test = id

-----------------------------------------------------------

data AorB
  = A
  | B
  deriving (Eq, Show)

type family JustDont2 (a :: AorB) :: * where
  JustDont2 A = Maybe String
  JustDont2 B = Int

data Constrained a = Constrained { unConstrained :: JustDont2 a }

test2 :: String -> Constrained 'A
test2 a = Constrained (Just a)
