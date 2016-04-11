{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module PublisherInfo
       ( PublisherInfo(..)
       ) where

import Data.Aeson
import Data.Text
import Data.Typeable
import GHC.Generics

import Publisher (PublisherId)

type PublisherName = Text

data PublisherInfo = PublisherInfo { publisherId :: PublisherId
                                   , name :: PublisherName
                                   }
                     deriving (Show, FromJSON, ToJSON, Generic, Typeable)
