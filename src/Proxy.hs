{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Proxy where

import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>))
import Data.Proxy (Proxy (Proxy))
import Data.Text (pack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

data Payload (s :: Symbol) a :: * where
  Payload :: a -> Payload (TypeKey a) a

data JsonMessage a where
  JsonMessage :: (s ~ TypeKey a, KnownSymbol s)
              => Payload s a
              -> JsonMessage a

unJsonMessage :: JsonMessage a -> a
unJsonMessage (JsonMessage (Payload a)) = a

type family TypeKey (a :: *) :: Symbol where
  TypeKey Int    = "int"
  TypeKey String = "string"
  TypeKey X      = "x"

-- | Instances for serializing Proxy
instance {-# OVERLAPPING #-} KnownSymbol s => ToJSON (Proxy s) where
  toJSON = A.String . pack . symbolVal

instance {-# OVERLAPPING #-} KnownSymbol s => FromJSON (Proxy s) where
  parseJSON (A.String s) | s == pack (symbolVal (Proxy :: Proxy s)) =
                            return (Proxy :: Proxy s)
  parseJSON _      = mzero

-- | ToJSON instance
instance (s ~ TypeKey a, KnownSymbol s, ToJSON a) => ToJSON (Payload s a) where
  toJSON (Payload a) =
    object [ "decodeToType" .= (Proxy :: Proxy s)
           , "data" .= a
           ]

-- | FromJSON instance
instance (s ~ TypeKey a, KnownSymbol s, FromJSON a) => FromJSON (Payload s a) where
  parseJSON (Object v) = (v .: "decodeToType" :: Parser (Proxy s))
                          >> Payload <$> v .: "data"
  parseJSON _          = mzero

-- | Show intance for ghci
instance (KnownSymbol s, Show a) => Show (Payload s a) where
  show (Payload a) = "Payload " <> symbolVal (Proxy :: Proxy s) <> " " <> show a


jsonString :: BL.ByteString
jsonString = "{\"type\": \"string\", \"data\": \"cool\"}"

x :: Payload "int" Int
x = Payload 10

data X = X { _first :: Int, _second :: String } deriving Show

$(deriveJSON defaultOptions ''X)

messageStringX :: BL.ByteString
messageStringX = "{\"decodeToType\": \"x\", \"data\": {\"_first\": 1, \"_second\": \"second\" } }"

messageStringB :: BL.ByteString
messageStringB = "{\"decodeToType\": \"string\", \"data\": 10}"

messageStringA :: BL.ByteString
messageStringA = "{\"decodeToType\": \"revenueData\", \"data\": \"cool\"}"

-- y :: JsonMessage Int
-- y = JsonMessage (Payload 10 :: Payload "int" Int)

-- data Env a = Env { m :: JsonMessage a }

-- instance FromJSON (JsonMessage a) => FromJSON (Env a) where
--   parseJSON (Object v) = Env <$> v .: "envelope"
--   parseJSON _          = mzero
