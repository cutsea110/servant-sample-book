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
import Data.Word
import GHC.Generics
import Servant.API (FromHttpApiData(..))

import Data.Scientific (scientific, coefficient)
import Types
import Address

newtype AuthorId = AuthorId { getAuthorId :: Word64 } deriving (Show, Generic, Typeable)

instance FromJSON AuthorId where
  parseJSON (Number v) = AuthorId <$> pure (fromIntegral $ coefficient v)
  parseJSON _ = mempty
instance ToJSON AuthorId where
  toJSON = Number . flip scientific 0 . toInteger . getAuthorId
instance FromHttpApiData AuthorId where
  parseQueryParam = Right . AuthorId . read . unpack

data Author = Author { authorId :: Maybe AuthorId
                     , name :: Text
                     , gender :: Gender
                     , birth :: Day
                     , age :: Word8
                     , address :: Address
                     , createdAt :: UTCTime
                     , updatedAt :: UTCTime
                     } deriving (Show, FromJSON, ToJSON, Generic, Typeable)

data AuthorQueryCondition
  = AuthorQueryCondition { authorNameLike :: Maybe Text
                         , genderEq :: Maybe Gender
                         , ageFrom :: Maybe Word8
                         , ageTo :: Maybe Word8
                         , prefectureIn :: Maybe [Prefecture]
                         } deriving (Show, FromJSON, ToJSON, Generic, Typeable)

data AuthorList = AuthorList { hits :: Word64
                             , page :: Word64
                             , per_page :: Word16
                             , result :: [Author]
                             } deriving (Show, FromJSON, ToJSON, Generic, Typeable)
