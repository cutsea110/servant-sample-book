{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Types
       ( Prefecture(..)
       , Postcode(..)
       , Tel(..)
       , Fax(..)
       , Emailaddress(..)
       , Gender(..)
       , CompanyType(..)
       , Category(..)
       , ISBN(..)
       )where

import GHC.Generics
import Data.Aeson
import Data.Text
import Servant.API

data Prefecture = Hokkaido
                | Aomori | Iwate | Miyagi | Akita | Yamagata | Fukushima
                | Ibaraki | Tochigi | Gunma | Saitama | Chiba | Tokyo | Kanagawa
                | Nigata | Toyama | Ishikawa | Fukui | Yamanashi | Nagano | Gifu | Shizuoka | Aichi
                | Mie | Shiga | Kyoto | Osaka | Hyogo | Nara | Wakayama
                | Tottori | Shimane | Okayama | Hiroshima | Yamaguchi
                | Tokushima | Kagawa | Ehime | Kochi
                | Fukuoka | Saga | Nagasaki | Kumamoto | Oita | Miyazaki | Kagoshima
                | Okinawa
                deriving (Show, FromJSON, ToJSON, Generic, Bounded, Enum)


newtype Postcode = Postcode { getPostcode :: Text } deriving (Show, Generic)

instance FromJSON Postcode where
  parseJSON (String v) = return $ Postcode v
  parseJSON _ = mempty
instance ToJSON Postcode where
  toJSON = String . getPostcode

newtype Tel = Tel { getTel :: Text } deriving (Show, Generic)

instance FromJSON Tel where
  parseJSON (String v) = return $ Tel v
  parseJSON _ = mempty
instance ToJSON Tel where
  toJSON = String . getTel

newtype Fax = Fax { getFax :: Text } deriving (Show, Generic)

instance FromJSON Fax where
  parseJSON (String v) = return $ Fax v
  parseJSON _ = mempty
instance ToJSON Fax where
  toJSON = String . getFax

newtype Emailaddress = Emailaddress { getEmailaddress :: Text } deriving (Show, Generic)

instance FromJSON Emailaddress where
  parseJSON (String v) = return $ Emailaddress v
  parseJSON _ = mempty
instance ToJSON Emailaddress where
  toJSON = String . getEmailaddress

data Gender = Female
            | Male
            deriving (Show, FromJSON, ToJSON, Generic, Bounded, Enum)

data CompanyType = CO
                 | INC
                 | COM
                 | LLC
                 | COLTD
                 deriving (Show, FromJSON, ToJSON, Generic, Bounded, Enum)

data Category = Science
              | Computer
              | Literate
              | Textbook
              | Comics
              | Magazine
              deriving (Show, FromJSON, ToJSON, Generic, Bounded, Enum)

newtype ISBN = ISBN {getISBN :: Text} deriving (Show, Generic)

instance FromJSON ISBN where
  parseJSON (String v) = return $ ISBN v
  parseJSON _ = mempty
instance ToJSON ISBN where
  toJSON = String . getISBN
instance FromHttpApiData ISBN where
  parseQueryParam = Right . ISBN

