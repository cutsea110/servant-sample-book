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
  let genDir = "gen/ServantClientBook/ServantClientBook"
  createDirectoryIfMissing True genDir
  content <- csForAPIWith def' api
  writeFile (genDir++"/API.cs") content
