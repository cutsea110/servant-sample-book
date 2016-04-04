{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Book
       ( Book(..)
       ) where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics

import Types
import Author
import Publisher

data Book = Book { title :: String
                 , isbn :: ISBN
                 , category :: Category
                 , description :: String
                 , publishedBy :: Publisher
                 , authers :: [Author]
                 , publishedAt :: Day
                 } deriving (Show, FromJSON, ToJSON, Generic)
