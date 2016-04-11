{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GenCSharp where

import Data.Monoid ((<>))
import Data.Text
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Typeable
import Data.Proxy
import Servant.Foreign

import Types
import Address
import Author
import AuthorInfo
import Publisher
import PublisherInfo
import Book
import API (api)

data CSharp

instance HasForeignType CSharp Text Int where
    typeFor _ _ _ = "int"
instance HasForeignType CSharp Text (Maybe Int) where
    typeFor _ _ _ = "int?"

instance HasForeignType CSharp Text Day where
    typeFor _ _ _ = "DateTime"
instance HasForeignType CSharp Text (Maybe Day) where
    typeFor _ _ _ = "DateTime?"

instance HasForeignType CSharp Text UTCTime where
    typeFor _ _ _ = "DateTime"
instance HasForeignType CSharp Text (Maybe UTCTime) where
    typeFor _ _ _ = "DateTime?"

instance HasForeignType CSharp Text Text where
    typeFor _ _ _ = "string"
instance HasForeignType CSharp Text (Maybe Text) where
    typeFor _ _ _ = "string"

instance HasForeignType CSharp Text String where
    typeFor _ _ _ = "string"
instance HasForeignType CSharp Text (Maybe String) where
    typeFor _ _ _ = "string"

instance HasForeignType CSharp Text Prefecture where
    typeFor _ _ _ = "Prefecture"

instance HasForeignType CSharp Text Postcode where
    typeFor _ _ _ = "Postcode"

instance HasForeignType CSharp Text Tel where
    typeFor _ _ _ = "Tel"

instance HasForeignType CSharp Text Fax where
    typeFor _ _ _ = "Fax"

instance HasForeignType CSharp Text Emailaddress where
    typeFor _ _ _ = "Emailaddress"

instance HasForeignType CSharp Text Gender where
    typeFor _ _ _ = "Gender"

instance HasForeignType CSharp Text CompanyType where
    typeFor _ _ _ = "CompanyType"

instance HasForeignType CSharp Text Category where
    typeFor _ _ _ = "Category"

instance HasForeignType CSharp Text ISBN where
    typeFor _ _ _ = "string"

instance HasForeignType CSharp Text Address where
    typeFor _ _ _ = "Address"
instance HasForeignType CSharp Text AddressId where
    typeFor _ _ _ = "int"

instance HasForeignType CSharp Text Author where
    typeFor _ _ _ = "Author"
instance HasForeignType CSharp Text AuthorId where
    typeFor _ _ _ = "int"
instance HasForeignType CSharp Text AuthorList where
    typeFor _ _ _ = "AuthorList"
instance HasForeignType CSharp Text AuthorQueryCondition where
    typeFor _ _ _ = "AuthorQueryCondition"

instance HasForeignType CSharp Text AuthorInfo where
    typeFor _ _ _ = "AuthorInfo"

instance HasForeignType CSharp Text Publisher where
    typeFor _ _ _ = "Publisher"
instance HasForeignType CSharp Text PublisherId where
    typeFor _ _ _ = "int"
instance HasForeignType CSharp Text PublisherList where
    typeFor _ _ _ = "PublisherList"
instance HasForeignType CSharp Text PublisherQueryCondition where
    typeFor _ _ _ = "PublisherQueryCondition"

instance HasForeignType CSharp Text PublisherInfo where
    typeFor _ _ _ = "PublisherInfo"

instance HasForeignType CSharp Text Book where
    typeFor _ _ _ = "Book"
instance HasForeignType CSharp Text BookId where
    typeFor _ _ _ = "int"
instance HasForeignType CSharp Text BookList where
    typeFor _ _ _ = "BookList"
instance HasForeignType CSharp Text BookQueryCondition where
    typeFor _ _ _ = "BookQueryCondition"

instance HasForeignType CSharp Text a => HasForeignType CSharp Text [a] where
    typeFor lang ftype (Proxy :: Proxy [t]) = "List<" <> typeFor lang ftype (Proxy :: Proxy t) <> ">"

instance HasForeignType CSharp Text () where
    typeFor _ _ _ = "void"

getEndpoints :: [Req Text]
getEndpoints = listFromAPI (Proxy :: Proxy CSharp) (Proxy :: Proxy Text) api

main :: IO ()
main = putStrLn "developing"
