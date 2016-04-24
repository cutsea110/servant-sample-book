{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Address
       ( AddressId(..)
       , Address(..)

       , AddressQueryCondition(..)
       , AddressList(..)
       ) where

import Data.Aeson
import Data.Scientific (scientific, coefficient)
import Data.Text (Text, unpack)
import Data.Time
import Data.Typeable
import Data.Word
import GHC.Generics
import Types
import Servant.API (FromHttpApiData(..))

newtype AddressId = AddressId { getAddressId :: Word64 } deriving (Show, Generic, Typeable)

instance FromJSON AddressId where
  parseJSON (Number v) = AddressId <$> pure (fromIntegral $ coefficient v)
  parseJSON _ = mempty
instance ToJSON AddressId where
  toJSON = Number . flip scientific 0 . toInteger . getAddressId
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
                       } deriving (Show, FromJSON, ToJSON, Generic, Typeable)

data AddressQueryCondition
    = AddressQueryCondition { postCodeLike :: Maybe Text
                            , telLike :: Maybe Text
                            , faxLike :: Maybe Text
                            , emailLike :: Maybe Text
                            } deriving (Show, FromJSON, ToJSON, Generic, Typeable)

data AddressList = AddressList { hits :: Word64
                               , page :: Word64
                               , per_page :: Word16
                               , result :: [Address]
                               } deriving (Show, FromJSON, ToJSON, Generic, Typeable)
