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
                | Nigata | Toyama | Ishikawa | Fukui | Yamanshi | Nagano | Gifu | Shizuoka | Aichi
                | Mie | Shiga | Kyoto | Osaka | Hyogo | Nara | Wakayama
                | Tottori | Shimane | Okayama | Hiroshima | Yamaguchi
                | Tokushima | Kagawa | Ehime | Kochi
                | Fukuoka | Saga | Nagasaki | Kumamoto | Oita | Miyazaki | Kagoshima
                | Okinawa
                deriving (Show, FromJSON, ToJSON, Generic, Bounded, Enum)


newtype Postcode = Postcode { getPostcode :: String } deriving (Show, FromJSON, ToJSON, Generic)
newtype Tel = Tel { getTel :: String } deriving (Show, FromJSON, ToJSON, Generic)
newtype Fax = Fax { getFax :: String } deriving (Show, FromJSON, ToJSON, Generic)
newtype Emailaddress = Emailaddress { getEmailaddress :: String } deriving (Show, FromJSON, ToJSON, Generic)
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

newtype ISBN = ISBN {getISBN :: Text} deriving (Show, FromJSON, ToJSON, Generic)

instance FromHttpApiData ISBN where
  parseQueryParam = Right . ISBN

