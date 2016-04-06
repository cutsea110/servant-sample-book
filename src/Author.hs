{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Author
       ( AuthorId(..)
       , Author(..)
       ) where

import Data.Aeson
import Data.Text (unpack)
import Data.Time (UTCTime)
import Data.Time.Calendar
import GHC.Generics
import Servant.API (FromHttpApiData(..))

import Data.Scientific (scientific, coefficient)
import Types
import Address

newtype AuthorId = AuthorId { getAuthorId :: Integer } deriving (Show, Generic)

instance FromJSON AuthorId where
  parseJSON (Number v) = AuthorId <$> pure (coefficient v)
  parseJSON _ = mempty
instance ToJSON AuthorId where
  toJSON = Number . flip scientific 0 . getAuthorId
instance FromHttpApiData AuthorId where
  parseQueryParam = Right . AuthorId . read . unpack

data Author = Author { authorId :: Maybe AuthorId
                     , name :: String
                     , gender :: Gender
                     , birth :: Day
                     , age :: Int
                     , address :: Address
                     , createdAt :: UTCTime
                     , updatedAt :: UTCTime
                     } deriving (Show, FromJSON, ToJSON, Generic)
