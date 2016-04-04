{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Book where

import Control.Monad.Trans.Except
import Data.Aeson
import Data.Text
import Data.Time.Calendar
import GHC.Generics
import Servant
import Servant.API
import Servant.Server
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

type Handler a = ExceptT ServantErr IO a

data Prefecture = Hokkaido
                | Aomori | Iwate | Miyagi | Akita | Yamagata | Fukushima
                | Ibaraki | Tochigi | Gunma | Saitama | Chiba | Tokyo | Kanagawa
                | Nigata | Toyama | Ishikawa | Fukui | Yamanshi | Nagano | Gifu | Shizuoka | Aichi
                | Mie | Shiga | Kyoto | Osaka | Hyogo | Nara | Wakayama
                | Tottori | Shimane | Okayama | Hiroshima | Yamaguchi
                | Tokushima | Kagawa | Ehime | Kochi
                | Fukuoka | Saga | Nagasaki | Kumamoto | Oita | Miyazaki | Kagoshima
                | Okinawa
                deriving (Show, FromJSON, ToJSON, Generic, Bounded, Enum)

newtype AddressId = AddressId { getAddressId :: Integer } deriving (Show, FromJSON, ToJSON, Generic)
newtype Postcode = Postcode { getPostcode :: String } deriving (Show, FromJSON, ToJSON, Generic)
newtype Tel = Tel { getTel :: String } deriving (Show, FromJSON, ToJSON, Generic)
newtype Fax = Fax { getFax :: String } deriving (Show, FromJSON, ToJSON, Generic)
newtype Emailaddress = Emailaddress { getEmailaddress :: String } deriving (Show, FromJSON, ToJSON, Generic)

data Address = Address { addressId :: AddressId
                       , postalcode :: Postcode
                       , prefecture :: Prefecture
                       , address :: String
                       , building :: String
                       , tel :: Tel
                       , fax :: Fax
                       , email :: Emailaddress
                       } deriving (Show, FromJSON, ToJSON, Generic)

               
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

data Gender = Male | Female deriving (Show, FromJSON, ToJSON, Generic, Bounded, Enum)

newtype AuthorId = AuthorId { getAuthorId :: Integer } deriving (Show, FromJSON, ToJSON, Generic)

data Author = Author { authorId :: AuthorId
                     , autherName :: String
                     , autherGender :: Gender
                     , autherBirth :: Day
                     , autherAge :: Int
                     , autherAddress :: Address
                     } deriving (Show, FromJSON, ToJSON, Generic)

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
       
data CompanyType = CO
                 | INC
                 | COM
                 | LLC
                 | COLTD
                 deriving (Show, FromJSON, ToJSON, Generic, Bounded, Enum)

newtype PublisherId = PublisherId { getPublisherId :: Integer } deriving (Show, FromJSON, ToJSON, Generic)

data Publisher = Publisher { publisherId :: PublisherId
                           , publisherName :: String
                           , publisherType :: CompanyType
                           , publisherAddress :: Address
                           } deriving (Show, FromJSON, ToJSON, Generic)

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
    
data Category = Science
              | Computer
              | Literate
              | Textbook
              | Comics
              | Magazine
              deriving (Show, FromJSON, ToJSON, Generic, Bounded, Enum)

newtype ISBN = ISBN {getISBN :: Text} deriving (Show, FromJSON, ToJSON, Generic)

instance FromHttpApiData ISBN where
  parseQueryParam = Right . ISBN

data Book = Book { bookTitle :: String
                 , bookISBN :: ISBN
                 , bookCategory :: Category
                 , bookDescription :: String
                 , bookPublishedBy :: Publisher
                 , bookAuthers :: [Author]
                 , bookPublishedAt :: Day
                 } deriving (Show, FromJSON, ToJSON, Generic)

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

app :: Application
app = serve api server

-- main :: IO ()
-- main = run 8081 $ serve api (mock api)
