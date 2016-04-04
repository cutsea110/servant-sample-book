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

data Book = Book { bookTitle :: String
                 , bookISBN :: ISBN
                 , bookCategory :: Category
                 , bookDescription :: String
                 , bookPublishedBy :: Publisher
                 , bookAuthers :: [Author]
                 , bookPublishedAt :: Day
                 } deriving (Show, FromJSON, ToJSON, Generic)
