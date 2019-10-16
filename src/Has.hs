{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Has where

import           Data.Has
import           Data.Set  (Set)
import qualified Data.Set  as S
import           Data.Text (Text)
import           Prelude
import qualified Prelude   as P

data Scene = Scene
  { backgroundImage   :: Text
  , characters        :: [Character]
  , bewilderedTourist :: Maybe Character
  , objects           :: [Either Rock WoodenCrate]
  }

data Character = Character
 { hat   :: Maybe DamageArray
 , head  :: DamageArray
 , torso :: DamageArray
 , legs  :: DamageArray
 , shoes :: Maybe DamageArray
 }

data DamageArray = DamageArray
  { noDamage        :: Text
  , someDamage      :: Text
  , excessiveDamage :: Text
  }

data Rock = Rock
  { weight    :: Double
  , rockImage :: Text
  }

data WoodenCrate = WoodenCrate
  { strength         :: Double
  , woodenCrateImage :: DamageArray
  }

collectImages :: Has Scene m => m -> Set Text
collectImages a =
  let Scene {..} = getter a
  in
    S.singleton backgroundImage
    <> mconcat (P.map collectCharacterImages characters)
    <> maybe mempty collectCharacterImages bewilderedTourist
    <> mconcat (P.map (either (S.singleton . collectRockImage)
                          collectWoodenCrateImages)
                  objects)

-- collectCharacterImages :: Character -> Set Text
-- collectCharacterImages = images
  -- =  maybe mempty collectDamageArrayImages hat
  -- <> collectDamageArrayImages head
  -- <> collectDamageArrayImages torso
  -- <> collectDamageArrayImages legs
  -- <> maybe mempty collectDamageArrayImages shoes

-- collectDamageArrayImages :: DamageArray -> Set Text
-- collectDamageArrayImages = images
  -- S.fromList
  -- [ noDamage
  -- , someDamage
  -- , excessiveDamage
  -- ]

collectRockImage :: Rock -> Text
collectRockImage Rock {..} = rockImage

collectWoodenCrateImages :: Has WoodenCrate m => m -> Set Text
collectWoodenCrateImages = getter
  -- collectDamageArrayImages woodenCrateImage

-- class HasImages a where
--   images :: a -> Set Text

-- instance HasImages a => HasImages [a] where
--   images xs = foldr (\x accum -> images x <> accum) mempty xs

-- instance HasImages a => HasImages (Maybe a) where
--   images x = maybe mempty images x

-- instance (HasImages a, HasImages b) => HasImages (Either a b) where
--   images x = either images images x

-- instance HasImages Scene where
--   images Scene {..}
--     =  S.singleton backgroundImage
--     <> images characters
--     <> images bewilderedTourist
--     <> images objects

-- instance HasImages Character where
--   images Character {..}
--     =  images hat
--     <> images head
--     <> images torso
--     <> images legs
--     <> images shoes

-- instance HasImages DamageArray where
--   images DamageArray {..} = S.fromList
--     [ noDamage
--     , someDamage
--     , excessiveDamage
--     ]

-- instance HasImages Rock where
--   images Rock {..} = S.singleton rockImage

-- instance HasImages WoodenCrate where
--   images WoodenCrate {..} = images woodenCrateImage
