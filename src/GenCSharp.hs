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


import Language.Haskell.Exts


def' :: GenerateCsConfig
def' = def { namespace = "ServantClientBook"
           , sources = [ "src/Types.hs"
                       , "src/Address.hs"
                       , "src/Author.hs"
                       , "src/Publisher.hs"
                       , "src/AuthorInfo.hs"
                       , "src/PublisherInfo.hs"
                       , "src/Book.hs"
                       ]
            }

main :: IO ()
main = genCsForAPI def' api
