{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GenCSharp where

import Prelude
import Control.Arrow
import Control.Lens
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack, intercalate, toLower)
import qualified Data.Text.IO as T
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

instance HasForeignType CSharp Text a => HasForeignType CSharp Text [a] where
    typeFor lang ftype (Proxy :: Proxy [t]) = "List<" <> typeFor lang ftype (Proxy :: Proxy t) <> ">"

instance HasForeignType CSharp Text a => HasForeignType CSharp Text (Maybe a) where
    typeFor lang ftype (Proxy :: Proxy (Maybe t)) = "Nullable<" <> typeFor lang ftype (Proxy :: Proxy t) <> ">"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text () where
    typeFor _ _ _ = "void"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text Int where
    typeFor _ _ _ = "int"
instance {-# OVERLAPPING #-} HasForeignType CSharp Text (Maybe Int) where
    typeFor _ _ _ = "int?"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text Text where
    typeFor _ _ _ = "string"
instance {-# OVERLAPPING #-} HasForeignType CSharp Text (Maybe Text) where
    typeFor _ _ _ = "string"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text String where
    typeFor _ _ _ = "string"
instance {-# OVERLAPPING #-} HasForeignType CSharp Text (Maybe String) where
    typeFor _ _ _ = "string"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text Day where
    typeFor _ _ _ = "DateTime"
instance {-# OVERLAPPING #-} HasForeignType CSharp Text (Maybe Day) where
    typeFor _ _ _ = "DateTime?"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text UTCTime where
    typeFor _ _ _ = "DateTime"
instance {-# OVERLAPPING #-} HasForeignType CSharp Text (Maybe UTCTime) where
    typeFor _ _ _ = "DateTime?"

instance {-# OVERLAPS #-} Typeable t => HasForeignType CSharp Text t where
    typeFor lang ftype p = pack $ show $ typeRep p

getEndpoints :: [Req Text]
getEndpoints = listFromAPI (Proxy :: Proxy CSharp) (Proxy :: Proxy Text) api

data CSharpOption = CSharpOption { prefix :: Text
                                 , jsonRequestBodyName ::Text
                                 } deriving Show
def :: CSharpOption
def = CSharpOption { prefix = "_"
                   , jsonRequestBodyName = "obj"
                   }

generateMethod :: CSharpOption -> Req Text -> Text
generateMethod opt req = "\n" <> aDecl <> aBody <> decl <> body
    where
      aDecl = "public async " <> retTaskTyp <> " " <> fnameAsync <> "(" <> argsDecl <> ")"
      aBody = " {\n" <> aStmts <> "}\n"

      decl = "public " <> retTyp <> " " <> fname <> "(" <> argsDecl <> ")"
      body = " {\n" <> stmts <> "}\n"

      retTaskTyp = case retTyp of
                     "void" -> "Task"
                     t -> "Task<" <> t <> ">"
      retTyp = fromJust $ req ^. reqReturnType
      fnameAsync = fname <> "Async"
      fname = req ^. reqFuncName . camelCaseL
      argsDecl = intercalate "," $ map (\(t, n) -> t <> " " <> n) argsTyp_Nm

      argsTyp_Nm :: [(Text, Text)]
      argsTyp_Nm = map (id *** (prefix opt <>)) argsTypNm
      argsTypNm, captures, rqBody, queryparams :: [(Text, Text)]
      argsTypNm = captures ++ rqBody ++ queryparams
      captures = map ((view argType &&& view argPath) . captureArg)
                 . filter isCapture
                 $ req ^. reqUrl . path
      rqBody = maybe [] (pure . (id &&& const (jsonRequestBodyName opt)))
               $ req ^. reqBody
      queryparams = map ((view argType &&& unPathSegment . view argName)
                        . view queryArgName)
                    $ req ^. reqUrl . queryStr

      aStmts = "    aStmts\n"
      stmts = "    stmts\n"

main :: IO ()
main = mapM_ (T.putStr . generateMethod def) getEndpoints
