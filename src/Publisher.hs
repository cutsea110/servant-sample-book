{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Publisher
       ( PublisherId(..)
       , Publisher(..)
       ) where

import Data.Aeson
import Data.Scientific (scientific, coefficient)
import GHC.Generics
import Types
import Address

newtype PublisherId = PublisherId { getPublisherId :: Integer } deriving (Show, Generic)

instance FromJSON PublisherId where
  parseJSON (Number v) = PublisherId <$> pure (coefficient v)
  parseJSON _ = mempty
instance ToJSON PublisherId where
  toJSON = Number . flip scientific 0 . getPublisherId

data Publisher = Publisher { publisherId :: Maybe PublisherId
                           , name :: String
                           , companyType :: CompanyType
                           , address :: Address
                           } deriving (Show, FromJSON, ToJSON, Generic)
