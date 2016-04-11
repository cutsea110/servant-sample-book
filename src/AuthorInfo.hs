{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module AuthorInfo
       ( AuthorInfo(..)
       ) where

import Data.Aeson
import Data.Text
import Data.Typeable
import GHC.Generics

import Author (AuthorId)

type AuthorName = Text

data AuthorInfo = AuthorInfo { authorId :: AuthorId
                             , name :: AuthorName
                             }
                  deriving (Show, FromJSON, ToJSON, Generic, Typeable)

