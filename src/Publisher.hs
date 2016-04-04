{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Publisher
       ( PublisherId(..)
       , Publisher(..)
       ) where

import Data.Aeson
import GHC.Generics
import Types
import Address

newtype PublisherId = PublisherId { getPublisherId :: Integer } deriving (Show, FromJSON, ToJSON, Generic)

data Publisher = Publisher { publisherId :: PublisherId
                           , name :: String
                           , companyType :: CompanyType
                           , address :: Address
                           } deriving (Show, FromJSON, ToJSON, Generic)
