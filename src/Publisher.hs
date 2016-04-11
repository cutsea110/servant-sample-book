{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Publisher
       ( PublisherId(..)
       , Publisher(..)

       , PublisherQueryCondition(..)
       , PublisherList(..)
       ) where

import Data.Aeson
import Data.Scientific (scientific, coefficient)
import Data.Text (Text, unpack)
import Data.Time (UTCTime)
import Data.Typeable
import GHC.Generics
import Servant.API (FromHttpApiData(..))

import Types
import Address

newtype PublisherId = PublisherId { getPublisherId :: Integer } deriving (Show, Generic, Typeable)

instance FromJSON PublisherId where
  parseJSON (Number v) = PublisherId <$> pure (coefficient v)
  parseJSON _ = mempty
instance ToJSON PublisherId where
  toJSON = Number . flip scientific 0 . getPublisherId
instance FromHttpApiData PublisherId where
  parseQueryParam = Right . PublisherId . read . unpack

data Publisher = Publisher { publisherId :: Maybe PublisherId
                           , name :: Text
                           , companyType :: CompanyType
                           , address :: Address
                           , createdAt :: UTCTime
                           , updatedAt :: UTCTime
                           } deriving (Show, FromJSON, ToJSON, Generic, Typeable)

data PublisherQueryCondition
  = PublisherQueryCondition { publisherNameLike :: Maybe Text
                            , companyTypeIn :: Maybe [CompanyType]
                            , prefectureIn :: Maybe [Prefecture]
                            } deriving (Show, FromJSON, ToJSON, Generic, Typeable)

data PublisherList = PublisherList { hits :: Integer
                                   , page :: Int
                                   , per_page :: Int
                                   , result :: [Publisher]
                                   } deriving (Show, FromJSON, ToJSON, Generic, Typeable)
