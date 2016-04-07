{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Address
       ( AddressId(..)
       , Address(..)
       ) where

import Data.Aeson
import Data.Scientific (scientific, coefficient)
import Data.Text (Text, unpack)
import Data.Time
import GHC.Generics
import Types
import Servant.API (FromHttpApiData(..))

newtype AddressId = AddressId { getAddressId :: Integer } deriving (Show, Generic)

instance FromJSON AddressId where
  parseJSON (Number v) = AddressId <$> pure (coefficient v)
  parseJSON _ = mempty
instance ToJSON AddressId where
  toJSON = Number . flip scientific 0 . getAddressId
instance FromHttpApiData AddressId where
  parseQueryParam = Right . AddressId . read . unpack

data Address = Address { addressId :: Maybe AddressId
                       , postcode :: Postcode
                       , prefecture :: Prefecture
                       , address :: Text
                       , building :: Text
                       , tel :: Tel
                       , fax :: Fax
                       , email :: Emailaddress
                       , createdAt :: UTCTime
                       , updatedAt :: UTCTime
                       } deriving (Show, FromJSON, ToJSON, Generic)
