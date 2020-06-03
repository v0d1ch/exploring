{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module BeautifulFolds where

import           Control.Lens (Getting, foldMapOf)
import qualified Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Set as Set
import           Prelude hiding (sum)

data Fold i o = forall m . Monoid m => Fold (i -> m) (m -> o)

data Average a = Average { numerator :: !a, denominator :: !Int }

newtype MapBySelector key m a = MapBySelector (Map key (m a)) deriving (Show)

instance Functor (Fold i) where
    fmap k (Fold tally summarize) = Fold tally (k . summarize)

instance Applicative (Fold i) where
    pure o = Fold (const ()) (const o)

    Fold tallyF summarizeF <*> Fold tallyX summarizeX = Fold tally summarize
      where
        tally i = (tallyF i, tallyX i)
        summarize (mF, mX) = summarizeF mF (summarizeX mX)

instance Num a => Semigroup (Average a) where
    (<>) (Average xL nL) (Average xR nR) = Average (xL + xR) (nL + nR)

instance Num a => Monoid (Average a) where
    mempty = Average 0 0
    mappend = (<>)

instance (Monoid (m a), Ord key) => Semigroup (MapBySelector key m a) where
    (<>) (MapBySelector m1) (MapBySelector m2) =
      let
        setOfKeys = Set.unions [ Set.fromList $ Map.keys m1, Set.fromList $ Map.keys m2 ]
        newContents =
          (\k ->
              let
                l1 = fromMaybe mempty $ Map.lookup k m1
                l2 = fromMaybe mempty $ Map.lookup k m2
              in
              (k, l1 <> l2)
          ) <$> Set.toList setOfKeys
      in
      MapBySelector $ Map.fromList newContents

instance (Monoid (m a), Ord key) => Monoid (MapBySelector key m a) where
  mempty = MapBySelector Map.empty
  mappend = (<>)

focus :: (forall m . Monoid m => Getting m b a) -> Fold a o -> Fold b o
focus lens (Fold tally summarize) = Fold (foldMapOf lens tally) summarize

foldFast :: Fold i o -> [i] -> o
foldFast (Fold tally summarize) is = summarize (reduce (map tally is))
  where
    reduce = Data.Foldable.foldl' (<>) mempty

sumFast :: Num n => Fold n n
sumFast = Fold Sum getSum

productFast :: Num n => Fold n n
productFast = Fold Product getProduct

lengthFast :: Num n => Fold i n
lengthFast = Fold (\_ -> Sum 1) getSum

-- Not a numerically stable average, but humor me
averageFast :: Fractional a => Fold a a
averageFast = Fold tally summarize
  where
    tally x = Average x 1

    summarize (Average numerator denominator) =
        numerator / fromIntegral denominator

groupFast
  :: (Ord key, Monoid (m a))
  => (a -> key)
  -> (a -> m a)
  -> Fold a (MapBySelector key m a)
groupFast keyf wrapf = Fold tally summarize
  where
    tally x = MapBySelector $ Map.singleton (keyf x) (wrapf x)

    summarize = id
