{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module GenCSharp where

import Prelude hiding (concat, lines, unlines)
import Control.Arrow
import Control.Lens
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BC (unpack)
import Data.Char (toUpper, toLower)
import Data.List (intercalate, concat)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text as T (Text, unpack, pack)
import qualified Data.Text.IO as T
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Typeable
import Data.Proxy
import Language.Haskell.Exts
import Network.HTTP.Types (Method(..))
import System.Directory (createDirectoryIfMissing)
import Text.Heredoc

import Servant.Foreign

import Types
import Address
import Author
import AuthorInfo
import Publisher
import PublisherInfo
import Book
import API (api)

import CS.JsonDotNet

def' :: GenerateCsConfig
def' = def { namespace = "ServantClientBook"
           , sources = [ "src/Types.hs"
                       , "src/Address.hs"
                       , "src/Author.hs"
                       , "src/Publisher.hs"
                       , "src/Book.hs"
                       ]
            }

main :: IO ()
main = do
  createDirectoryIfMissing True "gen/ServantClientBook/ServantClientBook"
  content <- csForAPIWith def' api
  writeFile "gen/ServantClientBook/ServantClientBook/API.cs" content
