{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GenCSharp where

import Prelude hiding (concat)
import Control.Arrow
import Control.Lens
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text as T (Text, pack, intercalate, toLower, concat)
import qualified Data.Text.IO as T
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Typeable
import Data.Proxy
import Network.HTTP.Types (Method(..))
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
      aBody = " {\n" <> concatWithIndent aStmts <> "}\n"

      concatWithIndent :: [Text] -> Text
      concatWithIndent = concat . map ("    " <>)

      decl = "public " <> retTyp <> " " <> fname <> "(" <> argsDecl <> ")"
      body = " {\n" <> concatWithIndent stmts <> "}\n"

      retTaskTyp = case retTyp of
                     "void" -> "Task"
                     t -> "Task<" <> t <> ">"
      retTyp = fromJust $ req^.reqReturnType
      fnameAsync = fname <> "Async"
      fname = req^.reqFuncName.camelCaseL
      argsDecl = intercalate ", " $ map (\(t, n) -> t <> " " <> n) argsTyp_Nm
      argsTyp_Nm :: [(Text, Text)]
      argsTyp_Nm = map (id *** (prefix opt <>)) argsTypNm
      argsTypNm :: [(Text, Text)]
      argsTypNm = captures ++ rqBody ++ map ((convToNullable *** (<>" = null")) . fst) queryparams
      captures :: [(Text, Text)]
      captures = map ((view argType &&& view argPath) . captureArg)
                 . filter isCapture
                 $ req^.reqUrl.path
      rqBody :: [(Text, Text)]
      rqBody = maybe [] (pure . (id &&& const (jsonRequestBodyName opt)))
               $ req^.reqBody
      queryparams :: [((Text, Text), ArgType)]
      queryparams = map (((view argType
                         &&&
                          unPathSegment . view argName)
                        . view queryArgName)
                         &&&
                         view queryArgType)
                    $ req^..reqUrl.queryStr.traverse

      aStmts = mkClient
               ++ mkQueryparam
               ++ mkJsonBody
               ++ doAsync
               ++ printReqRes
               ++ readContent
               ++ printContent
               ++ returnJson
      mkClient = ["var client = new ServantClient();\n"]
      mkQueryparam = if null queryparams then []
                     else [ "var queryparams = new List<string> {\n"] ++
                          mkQPStrs ++
                          [ "}.Where(e => !string.IsNullOrEmpty(e));\n"
                          , "var qp = queryparams.Count() > 0 ? $\"?{string.Join(\"&\", queryparams)}\" : \"\";\n" 
                          ]
      -- TODO: care about ArgType
      mkQPStrs = map (<>",\n") $ map mkQPStr queryparams
      mkQPStr ((_, n), _) = "   " <> n <> ".HasValue ? $\"" <> n <> "={" <> n <> ".Value}\" : null"
      mkJsonBody = if null rqBody then []
                   else [ "#if DEBUG\n"
                        , "var jsobObj = JsonConvert.SerializeObject(" <> objSym <> ", Formatting.Indented);\n"
                        , "#else\n"
                        , "var jsobObj = JsonConvert.SerializeObject(" <> objSym <> ");\n"
                        , "#endif\n"
                        ]
      doAsync = [ "var res = await client." <> asyncMethod <> "(" <> asyncParams <> ");"
                ]
      objSym = prefix opt <> jsonRequestBodyName opt
      asyncMethod = case req^.reqMethod of
                      "GET" -> "GetAsync"
                      "POST" -> "PostAsync"
                      "PUT" -> "PutAsync"
                      "DELETE" -> "DeleteAsync"
      asyncParams = intercalate ", " $ uri ++ strContent
      uri = [pre $ post
            $ segmentsToText (req^..reqUrl.path.traverse) <> qp
            ]
          where
            pre = ("$\"{server}"<>)
            post = (<>"\"")
            qp = if null (req^.reqUrl.queryStr) then ""
                 else "{qp}"
      segmentsToText :: [Segment f] -> Text
      segmentsToText = foldr segToText ""
          where
            segToText :: Segment f -> Text -> Text
            segToText (Segment (Static s)) ss
                = "/" <> s^._PathSegment <> ss
            segToText (Segment (Cap s)) ss
                = "/{"<> prefix opt <> s^.argName._PathSegment <> "}" <> ss
      strContent = if null rqBody then []
                   else ["new StringContent(jsonObj, Encoding.UTF8, \"application/json\")"]
      printReqRes = []
      readContent = []
      printContent = []
      returnJson = []

      stmts = [ retTaskTyp <> " t = " <> fnameAsync <> "(" <> argNames <> ");\n"
              , "return t.GetAwaiter().GetResult();\n"]
      argNames = intercalate ", " $ map snd argsTyp_Nm

      -- TODO: more typable
      convToNullable "int" = "int?"
      convToNullable "string" = "string"
      convToNullable "DateTime" = "DateTime?"
      convToNullable t = "Nullable<" <> t <> ">"

main :: IO ()
main = mapM_ (T.putStr . generateMethod def) getEndpoints
