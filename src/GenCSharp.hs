{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GenCSharp where

import Prelude hiding (concat, lines, unlines)
import Control.Arrow
import Control.Lens
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text as T (Text, pack, intercalate, toLower, concat, lines, unlines, isPrefixOf)
import qualified Data.Text.IO as T
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Typeable
import Data.Proxy
import Network.HTTP.Types (Method(..))
import System.Directory (createDirectoryIfMissing)

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
                                 , namespace :: Text
                                 } deriving Show
def :: CSharpOption
def = CSharpOption { prefix = "_"
                   , jsonRequestBodyName = "obj"
                   , namespace = "ServantClientAPI"
                   }

onToplevel :: Text -> Bool
onToplevel line = "\n" == line ||
                  isPrefixOf "#if" line ||
                  isPrefixOf "#elseif" line ||
                  isPrefixOf "#else" line

generateMethod :: CSharpOption -> Req Text -> Text
generateMethod opt req = "\n" <> aDecl <> aBody <> decl <> body
    where
      aDecl = "public async " <> retTaskTyp <> " " <> fnameAsync <> "(" <> argsDecl <> ")"
      aBody = " {\n" <> concatWithIndent aStmts <> "}\n"

      concatWithIndent :: [Text] -> Text
      concatWithIndent = concat . map indent
          where
            indent line = if onToplevel line
                          then line
                          else "    " <> line

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
      mkQPStr ((_, nm), _) = "   " <> _nm <> ".HasValue ? $\"" <> _nm <> "={" <> _nm <> ".Value}\" : null"
          where
            _nm = prefix opt <> nm
      mkJsonBody = if null rqBody then []
                   else [ "#if DEBUG\n"
                        , "var jsonObj = JsonConvert.SerializeObject(" <> objSym <> ", Formatting.Indented);\n"
                        , "#else\n"
                        , "var jsonObj = JsonConvert.SerializeObject(" <> objSym <> ");\n"
                        , "#endif\n"
                        ]
      doAsync = [ "var res = await client." <> asyncMethod <> "(" <> asyncParams <> ");\n"
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
                   else ["new StringContent(jsonObj, Encoding.UTF8, \"application/json\");\n"]
      printReqRes = ["Debug.WriteLine($\">>> {res.RequestMessage}\");\n"]
                    ++
                    if null rqBody then []
                    else ["Debug.WriteLine($\"-----\\n{jsonObj}\\n-----\");\n"]
                    ++
                    ["Debug.WriteLine($\"<<< {(int)res.StatusCode} {res.ReasonPhrase}\");\n"]
      readContent = ["var content = await res.Content.ReadAsStringAsync();\n"]
      printContent = ["Debug.WriteLine($\"<<< {content}\");\n"]
      returnJson = [rtn <> "JsonConvert.DeserializeObject" <> typ <> "(content);\n"]
          where
            (rtn, typ) = case retTyp of
                           "void" -> ("", "")
                           _      -> ("return ", "<" <> retTyp <> ">")

      stmts = [ retTaskTyp <> " t = " <> fnameAsync <> "(" <> argNames <> ");\n"
              , "return t.GetAwaiter().GetResult();\n"]
      argNames = intercalate ", " $ map snd argsTyp_Nm

      -- TODO: more typable
      convToNullable "int" = "int?"
      convToNullable "string" = "string"
      convToNullable "DateTime" = "DateTime?"
      convToNullable t = "Nullable<" <> t <> ">"


usingBlock :: [Text]
usingBlock = [ "using Newtonsoft.Json;\n"
             , "using System.Collections.Generic;\n"
             , "using System.Diagnostics;\n"
             , "using System.Linq\n"
             , "using System.Net.Http;\n"
             , "using System.Net.Http.Headers;\n"
             , "using System.Text\n"
             , "using System.Threading.Tasks\n"
             , "\n"
             , "#region type alias\n"
             , "using AddressId = System.Int64;\n"
             , "using AuthorId = System.Int64;\n"
             , "using PublisherId = System.Int64;\n"
             , "using BookId = System.Int64;\n"
             , "using ISBN = System.String;\n"
             , "using Postcode = System.String;\n"
             , "using Tel = System.String;\n"
             , "using Fax = System.String;\n"
             , "using Emailaddress = System.String;\n"
             , "#endregion\n"
             , "\n"
             ]

classHead :: CSharpOption -> [Text]
classHead opt = [ "namespace " <> namespace opt <> "\n"
                , "{\n"
                , "    class ServantClient : HttpClient\n"
                , "    {\n"
                , "        public ServantClient()\n"
                , "        {\n"
                , "            this.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue(\"application/json\"));\n"
                , "        }\n"
                , "    }\n"
                , "    public class API\n"
                , "    {\n"
                , "        #region Constructor\n"
                , "        private string server;\n"
                , "        public API(string _server)\n"
                , "        {\n"
                , "            this.server = _server;"
                , "        }\n"
                , "        #endregion\n"
                , "        #region API for server\n"
                ]

classTail :: [Text]
classTail     = [ "        #endregion\n"
                , "    }\n"
                , "}\n"
                ]

main :: IO ()
main = do
  createDirectoryIfMissing True "gen/ServantClientBook/ServantClientBook"
  T.writeFile "gen/ServantClientBook/ServantClientBook/API.cs" code
    where
      code :: Text
      code = concat fragments
      def' = def { namespace = "ServantClientBook" }
      fragments :: [Text]
      fragments = usingBlock ++
                  classHead def' ++
                  map indent generated ++
                  classTail
      generated :: [Text]
      generated = concatMap (lines . generateMethod def') getEndpoints
      indent :: Text -> Text
      indent line = if onToplevel line
                    then line <> "\n"
                    else "        " <> line <> "\n"
