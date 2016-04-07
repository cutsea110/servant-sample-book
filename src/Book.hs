{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Book
       ( BookId(..)
       , Book(..)
       ) where

import Data.Aeson
import Data.Scientific (scientific, coefficient)
import Data.Text (Text, unpack)
import Data.Time (UTCTime)
import Data.Time.Calendar
import GHC.Generics
import Servant.API (FromHttpApiData(..))

import Types
import AuthorInfo
import PublisherInfo

newtype BookId = BookId { getBookId :: Integer } deriving (Show, Generic)

instance FromJSON BookId where
  parseJSON (Number v) = BookId <$> pure (coefficient v)
  parseJSON _ = mempty
instance ToJSON BookId where
  toJSON = Number . flip scientific 0 . getBookId
instance FromHttpApiData BookId where
  parseQueryParam = Right . BookId . read . unpack

data Book = Book { bookId :: Maybe BookId
                 , title :: Text
                 , isbn :: ISBN
                 , category :: Category
                 , description :: Text
                 , publishedBy :: PublisherInfo
                 , authors :: [AuthorInfo]
                 , publishedAt :: Day
                 , createdAt :: UTCTime
                 , updatedAt :: UTCTime
                 } deriving (Show, FromJSON, ToJSON, Generic)
