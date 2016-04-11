{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Author
       ( AuthorId(..)
       , Author(..)

       , AuthorQueryCondition(..)
       , AuthorList(..)
       ) where

import Data.Aeson
import Data.Text (Text, unpack)
import Data.Time (UTCTime)
import Data.Time.Calendar
import Data.Typeable
import GHC.Generics
import Servant.API (FromHttpApiData(..))

import Data.Scientific (scientific, coefficient)
import Types
import Address

newtype AuthorId = AuthorId { getAuthorId :: Integer } deriving (Show, Generic, Typeable)

instance FromJSON AuthorId where
  parseJSON (Number v) = AuthorId <$> pure (coefficient v)
  parseJSON _ = mempty
instance ToJSON AuthorId where
  toJSON = Number . flip scientific 0 . getAuthorId
instance FromHttpApiData AuthorId where
  parseQueryParam = Right . AuthorId . read . unpack

data Author = Author { authorId :: Maybe AuthorId
                     , name :: Text
                     , gender :: Gender
                     , birth :: Day
                     , age :: Int
                     , address :: Address
                     , createdAt :: UTCTime
                     , updatedAt :: UTCTime
                     } deriving (Show, FromJSON, ToJSON, Generic, Typeable)

data AuthorQueryCondition
  = AuthorQueryCondition { authorNameLike :: Maybe Text
                         , genderEq :: Maybe Gender
                         , ageFrom :: Maybe Int
                         , ageTo :: Maybe Int
                         , prefectureIn :: Maybe [Prefecture]
                         } deriving (Show, FromJSON, ToJSON, Generic, Typeable)

data AuthorList = AuthorList { hits :: Integer
                             , page :: Int
                             , per_page :: Int
                             , result :: [Author]
                             } deriving (Show, FromJSON, ToJSON, Generic, Typeable)
