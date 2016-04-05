{-# LANGUAGE OverloadedStrings #-}
module Docs where

import Data.Maybe (fromJust)
import Data.Time.Calendar (Day, fromGregorian)
import Servant.API
import Servant.Docs

import Types
import Address
import Author
import Publisher
import Book

instance ToSample Integer where
  toSamples _ = [("test" , 42)]

instance ToSample ISBN where
  toSamples _ = [("ISBN", ISBN "isbn")]

instance ToSample Publisher where
  toSamples _ = [("publisher", Publisher (PublisherId 1) "Ohm" CO a)]
    where
      a = fromJust $ toSample undefined

instance ToSample Day where
  toSamples _ = [("day", fromGregorian 1970 11 6)]

instance ToSample Author where
  toSamples _ = [("author", Author (AuthorId 1) "Katsutoshi Itoh" Male d 45 a)]
    where
      a = fromJust $ toSample undefined
      d = fromJust $ toSample undefined

instance ToSample Address where
  toSamples _ = [("address", Address (AddressId 1) (Postcode "134-0091") Tokyo "Funabori, Edogawa" "Crest.F.SS" (Tel "090-4134-5069") (Fax "03-3356-7662") (Emailaddress "cutsea110@gmail.com"))]

instance ToSample Book where
  toSamples c = [("book", Book "Haskell book" (ISBN "isbn") Computer "for Haskeller" p [a] d)]
    where
      p = fromJust $ toSample undefined
      a = fromJust $ toSample undefined
      d = fromJust $ toSample undefined

instance ToParam (QueryParam "page" Int) where
  toParam _ = DocQueryParam "page" ["1", "2", "3", "10"] "page number to get" Normal
  
instance ToParam (QueryParam "per_page" Int) where
  toParam _ = DocQueryParam "per_page" ["10", "30", "50", "100"] "number of items per page to get" Normal
  

instance ToCapture (Capture "id" AddressId) where
  toCapture _ = DocCapture "id" "(integer) address numeric identifier"

instance ToCapture (Capture "id" AuthorId) where
  toCapture _ = DocCapture "id" "(integer) author numeric identifier"

instance ToCapture (Capture "id" PublisherId) where
  toCapture _ = DocCapture "id" "(integer) publisher numeric identifier"

instance ToCapture (Capture "isbn" ISBN) where
  toCapture _ = DocCapture "isbn" "(string) ISBN identifier"
