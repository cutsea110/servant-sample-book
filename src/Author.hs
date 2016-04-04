{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Author
       ( AuthorId(..)
       , Author(..)
       ) where

import Data.Aeson
import Data.Text
import Data.Time.Calendar
import GHC.Generics

import Types
import Address

newtype AuthorId = AuthorId { getAuthorId :: Integer } deriving (Show, FromJSON, ToJSON, Generic)

data Author = Author { authorId :: AuthorId
                     , name :: String
                     , gender :: Gender
                     , birth :: Day
                     , age :: Int
                     , address :: Address
                     } deriving (Show, FromJSON, ToJSON, Generic)

