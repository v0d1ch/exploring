{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Proxy where

-- import Control.Applicative ((<$>), (<*>))
-- import Control.Monad (mzero)
-- import Data.Aeson
-- import qualified Data.Aeson as A

-- -- | sum type containing all possible payloads
-- data Payloads
--        = FooPayload Int String
--        | InviteRequestPayload String
--        | IdentityValidationRequestPayload Int Int String

-- -- | dispatch on the value of `"type"`
-- instance FromJSON Payloads where
--   parseJSON (Object v) = v .: "type" >>= handlePayloadType
--     where
--       -- handle foo_payload key
--       handlePayloadType (A.String "foo_payload") =
--           v .: "data" >>= \sub ->
--             FooPayload <$> sub .: "id" <*> sub .: "msg"
--       -- handle invite_request key
--       handlePayloadType (A.String "invite_request") =
--           v .: "data" >>= \sub ->
--             InviteRequestPayload <$> sub .: "msg"
--       -- handle identity_validation_data key
--       handlePayloadType (A.String "identity_validation_data") =
--           v .: "data" >>= \sub ->
--             IdentityValidationRequestPayload
--               <$> sub .: "from"
--               <*> sub .: "to"
--               <*> sub .: "validation_msg"
--       -- default
--       handlePayloadType _ = mzero
--   parseJSON _         = mzero

-- import Control.Monad (mzero)
-- import Data.Aeson
-- import qualified Data.Aeson as A
-- import qualified Data.ByteString.Lazy as BL
-- import Data.Proxy (Proxy (Proxy))
-- import GHC.TypeLits (KnownSymbol, symbolVal)

-- instance ToJSON (Proxy "foo") where
--     toJSON p = object [ "type" .= symbolVal p ]

-- instance FromJSON (Proxy "foo") where
--     parseJSON (Object v) = v .: "type" >>= handleType
--       where
--         handleType (A.String "foo") = return (Proxy :: Proxy "foo")
--         handleType _                = mzero
--     parseJSON _      = mzero

-- jsonString :: BL.ByteString
-- jsonString = "{\"type\": \"foo\"}"
----------------------------------------------------------------------------------------------------
-- import Control.Applicative ((<$>))
-- import Control.Monad (mzero)
-- import Data.Aeson
-- import Data.Aeson.Types
-- import qualified Data.Aeson as A
-- import qualified Data.ByteString.Lazy as BL
-- import Data.Monoid ((<>))
-- import Data.Proxy (Proxy(Proxy))
-- import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
-- import Data.Text (pack)

-- -- | Instances for serializing Proxy
-- instance KnownSymbol s => ToJSON (Proxy s) where
--     toJSON = A.String . pack . symbolVal

-- instance KnownSymbol s => FromJSON (Proxy s) where
--   parseJSON (A.String s) | s == pack (symbolVal (Proxy :: Proxy s)) =
--                            return (Proxy :: Proxy s)
--   parseJSON _      = mzero

-- -- | our new data type.
-- newtype Payload (s :: Symbol) a = Payload a

-- -- | ToJSON instance
-- instance (KnownSymbol s, ToJSON a) => ToJSON (Payload s a) where
--   toJSON (Payload a) = object [ "type" .= (Proxy :: Proxy s)
--                               , "data" .= a
--                               ]

-- -- | FromJSON instance
-- instance (KnownSymbol s, FromJSON a) => FromJSON (Payload s a) where
--   parseJSON (Object v) = (v .: "type" :: Parser (Proxy s))
--                            >>
--                            Payload <$> v .: "data"
--     parseJSON _          = mzero

-- -- | Show intance for ghci
-- instance (KnownSymbol s, Show a) => Show (Payload s a) where
--   show (Payload a) = "Payload " <> symbolVal (Proxy :: Proxy s) <> " " <> show a

-- jsonString :: BL.ByteString
-- jsonString = "{\"type\": \"String\", \"data\": \"cool\"}"

-- import Control.Applicative ((<$>))
-- import Control.Monad (mzero)
-- import Data.Aeson
-- import Data.Aeson.Types
-- import qualified Data.Aeson as A
-- import qualified Data.ByteString.Lazy as BL
-- import Data.Monoid ((<>))
-- import Data.Proxy (Proxy(Proxy))
-- import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
-- import Data.Text (pack)

-- type family TypeKey (a :: *) :: Symbol where
--   TypeKey Int = "int"
--   TypeKey String = "string"

-- -- | Instances for serializing Proxy
-- instance KnownSymbol s => ToJSON (Proxy s) where
--   toJSON = A.String . pack . symbolVal

-- instance KnownSymbol s => FromJSON (Proxy s) where
--   parseJSON (A.String s) | s == pack (symbolVal (Proxy :: Proxy s)) = return (Proxy :: Proxy s)
--   parseJSON _      = mzero

-- -- | our new data type.
-- data Payload (s :: Symbol) a :: * where
--   Payload :: a -> Payload (TypeKey a) a

-- -- | ToJSON instance
-- instance (s ~ TypeKey a, KnownSymbol s, ToJSON a) => ToJSON (Payload s a) where
--   toJSON (Payload a) = object [ "type" .= (Proxy :: Proxy s)
--                               , "data" .= a
--                               ]

-- -- | FromJSON instance
-- instance (s ~ TypeKey a, KnownSymbol s, FromJSON a) => FromJSON (Payload s a) where
--   parseJSON (Object v) = (v .: "type" :: Parser (Proxy s))
--                          >>
--                          Payload <$> v .: "data"
--   parseJSON _          = mzero

-- -- | Show intance for ghci
-- instance (KnownSymbol s, Show a) => Show (Payload s a) where
--   show (Payload a) = "Payload " <> symbolVal (Proxy :: Proxy s) <> " " <> show a

-- jsonString :: BL.ByteString
-- jsonString = "{\"type\": \"string\", \"data\": \"cool\"}"

-- x :: Payload "int" Int
-- x = Payload 10
-----------------------------------------------------------------------------------------

import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Data.Text (pack)

type family TypeKey (a :: *) :: Symbol where
    TypeKey Int = "int"
    TypeKey String = "string"

-- | Instances for serializing Proxy
instance KnownSymbol s => ToJSON (Proxy s) where
    toJSON = A.String . pack . symbolVal

instance KnownSymbol s => FromJSON (Proxy s) where
    parseJSON (A.String s) | s == pack (symbolVal (Proxy :: Proxy s)) = return (Proxy :: Proxy s)
    parseJSON _      = mzero

-- | our new data type.
data Payload (s :: Symbol) a :: * where
    Payload :: a
            -> Payload (TypeKey a) a

-- | ToJSON instance
instance (s ~ TypeKey a, KnownSymbol s, ToJSON a) => ToJSON (Payload s a) where
    toJSON (Payload a) = object [ "type" .= (Proxy :: Proxy s)
                                , "data" .= a
                                ]

-- | FromJSON instance
instance (s ~ TypeKey a, KnownSymbol s, FromJSON a) => FromJSON (Payload s a) where
    parseJSON (Object v) = (v .: "type" :: Parser (Proxy s))
                           >>
                           Payload <$> v .: "data"
    parseJSON _          = mzero

-- | Show intance for ghci
instance (KnownSymbol s, Show a) => Show (Payload s a) where
    show (Payload a) = "Payload " <> symbolVal (Proxy :: Proxy s) <> " " <> show a

jsonString :: BL.ByteString
jsonString = "{\"type\": \"string\", \"data\": \"cool\"}"

x :: Payload "int" Int
x = Payload 10

data Message a where
    Message :: (s ~ TypeKey a, KnownSymbol s)
            => Payload s a
            -> Message a

instance ToJSON a => ToJSON (Message a) where
    toJSON (Message payload) = object [ "payload" .= payload ]

instance (s ~ TypeKey a, KnownSymbol s, FromJSON a) => FromJSON (Message a) where
    parseJSON (Object v) = Message <$> v .: "payload"
    parseJSON _ = mzero

instance Show a => Show (Message a) where
    show (Message p) = "Message ( " <> show p <> " )"

messageStringA :: BL.ByteString
messageStringA = "{ \"payload\": {\"type\": \"string\", \"data\": \"cool\"} }"

messageStringB :: BL.ByteString
messageStringB = "{ \"payload\": {\"type\": \"string\", \"data\": 10} }"

y :: Message Int
y = Message (Payload 10 :: Payload "int" Int)

data Env a = Env { m :: Message a }

instance FromJSON (Message a) => FromJSON (Env a) where
    parseJSON (Object v) = Env <$> v .: "envelope"
    parseJSON _ = mzero
