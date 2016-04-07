{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module API
       ( api
       , server
       ) where

import Control.Monad.Trans.Except
import Data.Aeson
import Data.Text
import Data.Time.Calendar
import GHC.Generics
import Servant
import Servant.API
import Servant.Server

import Types
import Address
import Author
import Publisher
import Book

type Handler a = ExceptT ServantErr IO a
               
type AddressAPI =
       "addresses" :> QueryParam "page" Int :> QueryParam "per_page" Int :> Get '[JSON] [Address]
  :<|> "address" :> ReqBody '[JSON] Address :> Post '[JSON] AddressId
  :<|> "address" :> Capture "id" AddressId :>
       (    Get '[JSON] Address
       :<|> ReqBody '[JSON] Address :> Put '[JSON] ()
       :<|> Delete '[JSON] ()
       )

addressServer :: (Maybe Int -> Maybe Int -> Handler [Address])
            :<|> (Address -> Handler AddressId)
            :<|> (AddressId ->
                       Handler Address
                  :<|> (Address -> Handler ())
                  :<|> Handler ()
                 )
addressServer = list :<|> new :<|> opes
  where
    list :: Maybe Int -> Maybe Int -> Handler [Address]
    list = undefined
    new :: Address -> Handler AddressId
    new = undefined
    opes :: AddressId -> Handler Address :<|> (Address -> Handler ()) :<|> Handler ()
    opes addressid = view addressid :<|> update addressid :<|> delete addressid
    view :: AddressId -> Handler Address
    view = undefined
    update :: AddressId -> Address -> Handler ()
    update = undefined
    delete :: AddressId -> Handler ()
    delete = undefined

type AuthorAPI =
       "authors" :> QueryParam "page" Int :> QueryParam "per_page" Int :> Get '[JSON] [Author]
  :<|> "author" :> ReqBody '[JSON] Author :> Post '[JSON] AuthorId
  :<|> "author" :> Capture "id" AuthorId :>
       (    Get '[JSON] Author
       :<|> ReqBody '[JSON] Author :> Put '[JSON] ()
       :<|> Delete '[JSON] ()
       )

authorServer :: (Maybe Int -> Maybe Int -> Handler [Author])
           :<|> (Author -> Handler AuthorId)
           :<|> (AuthorId ->
                      Handler Author
                 :<|> (Author -> Handler ())
                 :<|> Handler ()
                )
authorServer = list :<|> new :<|> opes
  where
    list :: Maybe Int -> Maybe Int -> Handler [Author]
    list = undefined
    new :: Author -> Handler AuthorId
    new = undefined
    opes :: AuthorId -> Handler Author :<|> (Author -> Handler ()) :<|> Handler ()
    opes authorid = view authorid :<|> update authorid :<|> delete authorid
    view :: AuthorId -> Handler Author
    view = undefined
    update :: AuthorId -> Author -> Handler ()
    update = undefined
    delete :: AuthorId -> Handler ()
    delete = undefined
       

type PublisherAPI =
       "publishers" :> QueryParam "page" Int :> QueryParam "per_page" Int :> Get '[JSON] [Publisher]
  :<|> "publisher" :> ReqBody '[JSON] Publisher :> Post '[JSON] PublisherId
  :<|> "publisher" :> Capture "id" PublisherId :>
       (    Get '[JSON] Publisher
       :<|> ReqBody '[JSON] Publisher :> Put '[JSON] ()
       :<|> Delete '[JSON] ()
       )

publisherServer :: (Maybe Int -> Maybe Int -> Handler [Publisher])
              :<|> (Publisher -> Handler PublisherId)
              :<|> (PublisherId ->
                        Handler Publisher
                   :<|> (Publisher -> Handler ())
                   :<|> Handler ()
                   )
publisherServer = list :<|> new :<|> opes
  where
    list :: Maybe Int -> Maybe Int -> Handler [Publisher]
    list = undefined
    new :: Publisher -> Handler PublisherId
    new = undefined
    opes :: PublisherId -> Handler Publisher :<|> (Publisher -> Handler ()) :<|> Handler ()
    opes publisherid = view publisherid :<|> update publisherid :<|> delete publisherid
    view :: PublisherId -> Handler Publisher
    view = undefined
    update :: PublisherId -> Publisher -> Handler ()
    update = undefined
    delete :: PublisherId -> Handler ()
    delete = undefined


type BookAPI =
       "books" :> QueryParam "page" Int :> QueryParam "per_page" Int :>
       (    Get '[JSON] BookList
       :<|> ReqBody '[JSON] BookQueryCondition :> Post '[JSON] BookList
       )
  :<|> "book" :> ReqBody '[JSON] Book :> Post '[JSON] BookId
  :<|> "book" :> Capture "id" BookId :>
       (    Get '[JSON] Book
       :<|> ReqBody '[JSON] Book :> Put '[JSON] ()
       :<|> Delete '[JSON] ()
       )
  :<|> "book" :> "isbn" :> Capture "isbn" ISBN :>
       (    Get '[JSON] Book
       :<|> ReqBody '[JSON] Book :> Put '[JSON] ()
       :<|> Delete '[JSON] ()
       )
bookServer :: (Maybe Int -> Maybe Int ->
                    Handler BookList
               :<|> (BookQueryCondition -> Handler BookList)
              )
         :<|> (Book -> Handler BookId)
         :<|> (BookId ->
                    Handler Book
               :<|> (Book -> Handler ())
               :<|> Handler ()
               )
         :<|> (ISBN ->
                    Handler Book
               :<|> (Book -> Handler ())
               :<|> Handler ()
               )
bookServer = list :<|> new :<|> opes :<|> opes'
  where
    list :: Maybe Int -> Maybe Int -> Handler BookList :<|> (BookQueryCondition -> Handler BookList)
    list page per_page = get page per_page :<|> finder page per_page
    get :: Maybe Int -> Maybe Int -> Handler BookList
    get = undefined
    finder :: Maybe Int -> Maybe Int -> BookQueryCondition -> Handler BookList
    finder = undefined
    new :: Book -> Handler BookId
    new = undefined
    opes :: BookId -> Handler Book :<|> (Book -> Handler ()) :<|> Handler ()
    opes bookid = view bookid :<|> update bookid :<|> delete bookid
    view :: BookId -> Handler Book
    view = undefined
    update :: BookId -> Book -> Handler ()
    update = undefined
    delete :: BookId -> Handler ()
    delete = undefined
    opes' :: ISBN -> Handler Book :<|> (Book -> Handler ()) :<|> Handler ()
    opes' isbn = view' isbn :<|> update' isbn :<|> delete' isbn
    view' :: ISBN -> Handler Book
    view' = undefined
    update' :: ISBN -> Book -> Handler ()
    update' = undefined
    delete' :: ISBN -> Handler ()
    delete' = undefined

type API = AddressAPI
      :<|> AuthorAPI
      :<|> PublisherAPI
      :<|> BookAPI

api :: Proxy API
api = Proxy

server :: Server API
server = addressServer
    :<|> authorServer
    :<|> publisherServer
    :<|> bookServer
