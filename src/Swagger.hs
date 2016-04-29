{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Swagger where

import Control.Arrow ((***))
import Data.Aeson (encode)
import Data.Monoid
import Data.Proxy
import Data.Scientific
import Data.Swagger
import Data.Swagger.Internal.Schema
import Data.Time
import Data.Time.Clock
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Word
import Servant.Swagger

import Types
import Address
import Author
import AuthorInfo
import Publisher
import PublisherInfo
import Book
import API (api)

word64Min, word64Max :: Scientific
(word64Min, word64Max)
    = tapp conv ((minBound, maxBound) :: (Word64, Word64))
    where
      tapp f = f *** f
      conv = fromInteger . toInteger

instance ToParamSchema ISBN where
    toParamSchema _ = mempty
                      { _paramSchemaType = SwaggerString
                      }
instance ToSchema ISBN where
    declareNamedSchema = pure . named "ISBN" . paramSchemaToSchema

instance ToParamSchema AddressId where
    toParamSchema _ = mempty
                      { _paramSchemaType = SwaggerInteger
                      , _paramSchemaMinimum = Just word64Min
                      , _paramSchemaMaximum = Just word64Max
                      }
instance ToSchema AddressId where
    declareNamedSchema = pure . named "AddressId" . paramSchemaToSchema

instance ToParamSchema AuthorId where
    toParamSchema _ = mempty
                      { _paramSchemaType = SwaggerInteger
                      , _paramSchemaMinimum = Just word64Min
                      , _paramSchemaMaximum = Just word64Max
                      }

instance ToSchema AuthorId where
    declareNamedSchema = pure . named "AuthorId" . paramSchemaToSchema

instance ToParamSchema PublisherId where
    toParamSchema _ = mempty
                      { _paramSchemaType = SwaggerInteger
                      , _paramSchemaMinimum = Just word64Min
                      , _paramSchemaMaximum = Just word64Max
                      }

instance ToSchema PublisherId where
    declareNamedSchema = pure . named "PublisherId" . paramSchemaToSchema

instance ToParamSchema BookId where
    toParamSchema _ = mempty
                      { _paramSchemaType = SwaggerInteger
                      , _paramSchemaMinimum = Just word64Min
                      , _paramSchemaMaximum = Just word64Max
                      }
instance ToSchema BookId where
    declareNamedSchema = pure . named "BookId" . paramSchemaToSchema

instance ToParamSchema Emailaddress where
    toParamSchema _ = mempty
                      { _paramSchemaType = SwaggerString
                      }
instance ToSchema Emailaddress where
    declareNamedSchema = pure . named "Emailaddress" . paramSchemaToSchema

instance ToParamSchema Fax where
    toParamSchema _ = mempty
                      { _paramSchemaType = SwaggerString
                      }
instance ToSchema Fax where
    declareNamedSchema = pure . named "Fax" . paramSchemaToSchema

instance ToParamSchema Tel where
    toParamSchema _ = mempty
                      { _paramSchemaType = SwaggerString
                      }
instance ToSchema Tel where
    declareNamedSchema = pure . named "Tel" . paramSchemaToSchema

instance ToParamSchema Postcode where
    toParamSchema _ = mempty
                      { _paramSchemaType = SwaggerString
                      }
instance ToSchema Postcode where
    declareNamedSchema = pure . named "Postcode" . paramSchemaToSchema

instance ToSchema Prefecture
instance ToSchema Category
instance ToSchema CompanyType
instance ToSchema Gender

instance ToSchema Address
instance ToSchema AddressList
instance ToSchema AddressQueryCondition

instance ToSchema Author
instance ToSchema AuthorList
instance ToSchema AuthorInfo
instance ToSchema AuthorQueryCondition

instance ToSchema Publisher
instance ToSchema PublisherList
instance ToSchema PublisherInfo
instance ToSchema PublisherQueryCondition

instance ToSchema Book
instance ToSchema BookList
instance ToSchema BookQueryCondition

main :: IO ()
main = BLC.writeFile "docs/swagger.json" (encode $ toSwagger api)
