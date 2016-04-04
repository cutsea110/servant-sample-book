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

import Model

type Handler a = ExceptT ServantErr IO a
               
type AddressAPI =
       "addresses" :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [Address]
  :<|> "address" :> ReqBody '[JSON] Address :> Post '[JSON] Integer
  :<|> "address" :> Capture "id" Integer :>
       (    Get '[JSON] Address
       :<|> ReqBody '[JSON] Address :> Put '[JSON] ()
       :<|> Delete '[JSON] ()
       )

addressServer :: (Maybe Int -> Maybe Int -> Handler [Address])
            :<|> (Address -> Handler Integer)
            :<|> (Integer ->
                       Handler Address
                  :<|> (Address -> Handler ())
                  :<|> Handler ()
                 )
addressServer = list :<|> new :<|> opes
  where
    list :: Maybe Int -> Maybe Int -> Handler [Address]
    list = undefined
    new :: Address -> Handler Integer
    new = undefined
    opes :: Integer -> Handler Address :<|> (Address -> Handler ()) :<|> Handler ()
    opes addressid = view addressid :<|> update addressid :<|> delete addressid
    view :: Integer -> Handler Address
    view = undefined
    update :: Integer -> Address -> Handler ()
    update = undefined
    delete :: Integer -> Handler ()
    delete = undefined

type AuthorAPI =
       "authors" :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [Author]
  :<|> "author" :> ReqBody '[JSON] Author :> Post '[JSON] Integer
  :<|> "author" :> Capture "id" Integer :>
       (    Get '[JSON] Author
       :<|> ReqBody '[JSON] Author :> Put '[JSON] ()
       :<|> Delete '[JSON] ()
       )

authorServer :: (Maybe Int -> Maybe Int -> Handler [Author])
           :<|> (Author -> Handler Integer)
           :<|> (Integer ->
                      Handler Author
                 :<|> (Author -> Handler ())
                 :<|> Handler ()
                )
authorServer = list :<|> new :<|> opes
  where
    list :: Maybe Int -> Maybe Int -> Handler [Author]
    list = undefined
    new :: Author -> Handler Integer
    new = undefined
    opes :: Integer -> Handler Author :<|> (Author -> Handler ()) :<|> Handler ()
    opes authorid = view authorid :<|> update authorid :<|> delete authorid
    view :: Integer -> Handler Author
    view = undefined
    update :: Integer -> Author -> Handler ()
    update = undefined
    delete :: Integer -> Handler ()
    delete = undefined
       

type PublisherAPI =
       "publishers" :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [Publisher]
  :<|> "publisher" :> ReqBody '[JSON] Publisher :> Post '[JSON] Integer
  :<|> "publisher" :> Capture "id" Integer :>
       (    Get '[JSON] Publisher
       :<|> ReqBody '[JSON] Publisher :> Put '[JSON] ()
       :<|> Delete '[JSON] ()
       )

publisherServer :: (Maybe Int -> Maybe Int -> Handler [Publisher])
              :<|> (Publisher -> Handler Integer)
              :<|> (Integer ->
                        Handler Publisher
                   :<|> (Publisher -> Handler ())
                   :<|> Handler ()
                   )
publisherServer = list :<|> new :<|> opes
  where
    list :: Maybe Int -> Maybe Int -> Handler [Publisher]
    list = undefined
    new :: Publisher -> Handler Integer
    new = undefined
    opes :: Integer -> Handler Publisher :<|> (Publisher -> Handler ()) :<|> Handler ()
    opes publisherid = view publisherid :<|> update publisherid :<|> delete publisherid
    view :: Integer -> Handler Publisher
    view = undefined
    update :: Integer -> Publisher -> Handler ()
    update = undefined
    delete :: Integer -> Handler ()
    delete = undefined


type BookAPI =
       "books" :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [Book]
  :<|> "book" :> ReqBody '[JSON] Book :> Post '[JSON] ISBN
  :<|> "book" :> Capture "isbn" ISBN :>
       (    Get '[JSON] Book
       :<|> ReqBody '[JSON] Book :> Put '[JSON] ()
       :<|> Delete '[JSON] ()
       )

bookServer :: (Maybe Int -> Maybe Int -> Handler [Book])
         :<|> (Book -> Handler ISBN)
         :<|> (ISBN ->
                    Handler Book
               :<|> (Book -> Handler ())
               :<|> Handler ()
               )
bookServer = list :<|> new :<|> opes
  where
    list :: Maybe Int -> Maybe Int -> Handler [Book]
    list = undefined
    new :: Book -> Handler ISBN
    new = undefined
    opes :: ISBN -> Handler Book :<|> (Book -> Handler ()) :<|> Handler ()
    opes isbn = view isbn :<|> update isbn :<|> delete isbn
    view :: ISBN -> Handler Book
    view = undefined
    update :: ISBN -> Book -> Handler ()
    update = undefined
    delete :: ISBN -> Handler ()
    delete = undefined

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
