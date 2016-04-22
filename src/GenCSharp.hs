{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module GenCSharp where

import System.Directory (createDirectoryIfMissing)

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
