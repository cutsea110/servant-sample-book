module Address
       ( AddressId(..)
       , Address(..)
       ) where

import Data.Aeson
import GHC.Generics
import Types

newtype AddressId = AddressId { getAddressId :: Integer } deriving (Show, FromJSON, ToJSON, Generic)

data Address = Address { addressId :: AddressId
                       , postalcode :: Postcode
                       , prefecture :: Prefecture
                       , address :: String
                       , building :: String
                       , tel :: Tel
                       , fax :: Fax
                       , email :: Emailaddress
                       } deriving (Show, FromJSON, ToJSON, Generic)

