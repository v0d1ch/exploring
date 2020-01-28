{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, DeriveGeneric, FlexibleContexts, KindSignatures, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}

module Sop where

import Generics.SOP
import Data.Kind (Type)
​
import qualified GHC.Generics (Generic)
​
​
data InterestOrPrincipal
  = InterestTransaction
  | PrincipalTransaction
  deriving (GHC.Generics.Generic, Show)
instance Generic InterestOrPrincipal
​
data RevenueTypeSelector
  = RevenueFooSelector
  | RevenueBarSelector
  | RevenueBazSelector
  deriving (GHC.Generics.Generic, Show)
instance Generic RevenueTypeSelector
​
data IcoreBankTransactionCategory
  = TransactionInterestOrPrincipal InterestOrPrincipal
  | TransactionRevenueCategory RevenueTypeSelector
  | NotYetCategorized
  deriving (GHC.Generics.Generic, Show)
instance Generic IcoreBankTransactionCategory
​
​
type Enumerable x = (Generic x, GEnumerableS (Code x))
​
enumerate :: forall a. Enumerable a => [a]
enumerate = map (to . SOP) $ genumerateS @(Code a)
​
​
class GEnumerableS (xss :: [[Type]]) where
  genumerateS :: [NS (NP I) xss]
​
instance GEnumerableS '[] where
  genumerateS = []
​
instance (GEnumerableP xs, GEnumerableS xss) => GEnumerableS (xs ': xss) where
  genumerateS = (Z <$> genumerateP @xs) ++ (S <$> genumerateS @xss)
​
​
class GEnumerableP (xs :: [Type]) where
  genumerateP :: [NP I xs]
​
instance GEnumerableP '[] where
  genumerateP = pure Nil
​
instance (Enumerable x, GEnumerableP xs) => GEnumerableP (x ': xs) where
  genumerateP = (:*) <$> (I <$> enumerate @x) <*> genumerateP @xs
​
​
categoriesList :: [IcoreBankTransactionCategory]
categoriesList = enumerate
