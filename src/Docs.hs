{-# LANGUAGE OverloadedStrings #-}
module Docs where

import Data.Maybe (fromJust)
import Data.Time (UTCTime(..))
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (DiffTime(..), secondsToDiffTime)
import Servant.API
import Servant.Docs

import Types
import Address
import Author
import Publisher
import Book
import API (api)

instance ToSample Integer where
  toSamples _ = [("test" , 42)]

instance ToSample AddressId where
  toSamples _ = [("addressId", AddressId 42)]

instance ToSample AuthorId where
  toSamples _ = [("authorId", AuthorId 42)]

instance ToSample PublisherId where
  toSamples _ = [("publisherId", PublisherId 42)]

instance ToSample BookId where
  toSamples _ = [("bookId", BookId 42)]

instance ToSample ISBN where
  toSamples _ = [("ISBN", ISBN "isbn")]

instance ToSample Publisher where
  toSamples _ = [("publisher", Publisher (Just (PublisherId 1)) "Ohm" CO a d1 d2)]
    where
      a = fromJust $ toSample undefined
      d1 = fromJust $ toSample undefined
      d2 = fromJust $ toSample undefined

instance ToSample DiffTime where
  toSamples _ = [("difftime", secondsToDiffTime s)]
    where
      s = fromJust $ toSample undefined

instance ToSample UTCTime where
  toSamples _ = [("utctime", UTCTime d t)]
    where
      d = fromJust $ toSample undefined
      t = fromJust $ toSample undefined

instance ToSample Day where
  toSamples _ = [("day", fromGregorian 1970 11 6)]

instance ToSample Author where
  toSamples _ = [("author", Author (Just (AuthorId 1)) "Katsutoshi Itoh" Male d 45 a d1 d2)]
    where
      a = fromJust $ toSample undefined
      d = fromJust $ toSample undefined
      d1 = fromJust $ toSample undefined
      d2 = fromJust $ toSample undefined

instance ToSample Address where
  toSamples _ = [("address", Address (Just (AddressId 1)) (Postcode "134-0091") Tokyo "Funabori, Edogawa" "Crest.F.SS" (Tel "090-4134-5069") (Fax "03-3356-7662") (Emailaddress "cutsea110@gmail.com") d1 d2)]
    where
      d1 = fromJust $ toSample undefined
      d2 = fromJust $ toSample undefined

instance ToSample Book where
  toSamples c = [("book", Book (Just (BookId 1)) "Haskell book" (ISBN "isbn") Computer "for Haskeller" p [a] d d1 d2)]
    where
      p = fromJust $ toSample undefined
      a = fromJust $ toSample undefined
      d = fromJust $ toSample undefined
      d1 = fromJust $ toSample undefined
      d2 = fromJust $ toSample undefined

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
  toCapture _ = DocCapture "isbn" "(string) book ISBN identifier"

instance ToCapture (Capture "id" BookId) where
  toCapture _ = DocCapture "id" "(integer) book numeric identifier"

genDoc :: IO ()
genDoc = putStr $ markdown $ docs $ api
