{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Book
       ( BookId(..)
       , Book(..)
         
       , BookQueryCondition(..)
       , BookList(..)
       ) where

import Data.Aeson
import Data.Scientific (scientific, coefficient)
import Data.Text (Text, unpack)
import Data.Time (UTCTime)
import Data.Time.Calendar
import Data.Typeable
import Data.Word
import GHC.Generics
import Servant.API (FromHttpApiData(..))

import Types
import AuthorInfo
import PublisherInfo

newtype BookId = BookId { getBookId :: Word64 } deriving (Show, Generic, Typeable)

instance FromJSON BookId where
  parseJSON (Number v) = BookId <$> pure (fromInteger $ coefficient v)
  parseJSON _ = mempty
instance ToJSON BookId where
  toJSON = Number . flip scientific 0 . toInteger . getBookId
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
                 } deriving (Show, FromJSON, ToJSON, Generic, Typeable)

data BookQueryCondition
  = BookQueryCondition { bookIdEq :: Maybe BookId
                       , bookIdIn :: Maybe [BookId]
                       , isbnEq :: Maybe ISBN
                       , categoryIn :: Maybe [Category]
                       , authorNameLike :: Maybe Text
                       , publisherNameLike :: Maybe Text
                       , publishedFrom :: Maybe Day
                       , publishedTo :: Maybe Day
                       } deriving (Show, FromJSON, ToJSON, Generic, Typeable)

data BookList = BookList { hits :: Word64
                         , page :: Word64
                         , per_page :: Word16
                         , result :: [Book]
                         } deriving (Show, FromJSON, ToJSON, Generic, Typeable)
