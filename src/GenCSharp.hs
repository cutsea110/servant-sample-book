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
import qualified Data.List as DL (intercalate)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text as T (Text, pack, unpack, intercalate, toLower, concat, lines, unlines, isPrefixOf)
import qualified Data.Text.IO as T
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Typeable
import Data.Proxy
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
                  "" == line || -- care for after `lines`
                  isPrefixOf "#if" line ||
                  isPrefixOf "#else" line ||
                  isPrefixOf "#elif" line ||
                  isPrefixOf "#endif" line

generateMethod :: CSharpOption -> Req Text -> Text
generateMethod opt req = "\n" <> aDecl <> aBody <> decl <> body
    where
      aDecl = "public async " <> retTaskTyp <> " " <> fnameAsync <> "(" <> argsDecl <> ")\n"
      aBody = "{\n" <> concatWithIndent aStmts <> "}\n"

      concatWithIndent :: [Text] -> Text
      concatWithIndent = concat . map indent
          where
            indent line = if onToplevel line
                          then line
                          else "    " <> line

      decl = "public " <> retTyp <> " " <> fname <> "(" <> argsDecl <> ")\n"
      body = "{\n" <> concatWithIndent stmts <> "}\n"

      retTaskTyp = case retTyp of
                     "void" -> "Task"
                     t -> "Task<" <> t <> ">"
      retTyp = fromJust $ req^.reqReturnType
      fnameAsync = fname <> "Async"
      fname = req^.reqFuncName.camelCaseL
      argsDecl = T.intercalate ", " $ map (\(t, n) -> t <> " " <> n) argsTyp_Nm
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
      asyncParams = T.intercalate ", " $ uri ++ strContent
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
              , rtn <> "t.GetAwaiter().GetResult();\n"]
          where
            rtn = case retTyp of
                    "void" -> ""
                    _ -> "return "
      argNames = T.intercalate ", " $ map snd argsTyp_Nm

      -- TODO: more typable
      convToNullable "int" = "int?"
      convToNullable "string" = "string"
      convToNullable "DateTime" = "DateTime?"
      convToNullable t = "Nullable<" <> t <> ">"


usingBlock :: [Text]
usingBlock = [ "using Newtonsoft.Json;\n"
             , "using System.Collections.Generic;\n"
             , "using System.Diagnostics;\n"
             , "using System.Linq;\n"
             , "using System.Net.Http;\n"
             , "using System.Net.Http.Headers;\n"
             , "using System.Text;\n"
             , "using System.Threading.Tasks;\n"
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

-----
-- | TODO : rename
namespace' :: String
namespace' = "ServantClientBook"

-- | TODO : more typeable
types :: [(String, String)]
types = [ ("AddressId",    "System.Int64")
        , ("AuthorId",     "System.Int64")
        , ("PublisherId",  "System.Int64")
        , ("BookId",       "System.Int64")
        , ("ISBN",         "System.String")
        , ("Postcode",     "System.String")
        , ("Tel",          "System.String")
        , ("Fax",          "System.String")
        , ("Emailaddress", "System.String")
        ]

retType :: Req Text -> String
retType req = T.unpack $ fromJust $ req^.reqReturnType

uri :: Req Text -> String
uri req = T.unpack $ segmentsToText $ req^..reqUrl.path.traverse
    where
      segmentsToText :: [Segment f] -> Text
      segmentsToText = foldr segToText ""
      segToText :: Segment f -> Text -> Text
      segToText (Segment (Static s)) ss
          = "/" <> s^._PathSegment <> ss
      segToText (Segment (Cap s)) ss
          = "/{" <> prefix <> s^.argName._PathSegment <> "}" <> ss
      prefix = "_"

methodType :: Req Text -> String
methodType req = case req^.reqMethod of
                   "GET" -> "Get"
                   "POST" -> "Post"
                   "PUT" -> "Put"
                   "DELETE" -> "Delete"

methodName :: Req Text -> String
methodName req = T.unpack $ req^.reqFuncName.camelCaseL

paramDecl :: Req Text -> String
paramDecl req = DL.intercalate ", " $ map help $ paramInfos req
    where
      help :: (String, String) -> String
      help (t, n) = t ++ " " ++ ("_"++n)

paramArg :: Req Text -> String
paramArg req = DL.intercalate ", " $ map fst $ paramInfos req

paramInfos :: Req Text -> [(String, String)]
paramInfos req = captures req ++ rqBody req ++ map help (queryparams req)
    where
      help = convToNullable *** (++" = null")
      -- TODO : more typeable
      convToNullable "int" = "int?"
      convToNullable "string" = "string"
      convToNullable "DateTime" = "DateTime?"
      convToNullable t = "Nullable<"++t++">"

queryparams :: Req Text -> [(String, String)]
queryparams req = map ((T.unpack . view argType
                       &&&
                        T.unpack . unPathSegment . view argName)
                      . view queryArgName)
                  $ req^..reqUrl.queryStr.traverse

captures :: Req Text -> [(String, String)]
captures req = map ((T.unpack . view argType &&& T.unpack . view argPath)
                    . captureArg)
               . filter isCapture
               $ req^.reqUrl.path

rqBody :: Req Text -> [(String, String)]
rqBody req = maybe [] (pure . (T.unpack &&& const jsonReqBodyName)) $ req^.reqBody
    where
      jsonReqBodyName = "obj"

requestBodyExists :: Req Text -> Bool
requestBodyExists = null . rqBody

template :: String
template = [heredoc|/* generated by servant-csharp */
using Newtonsoft.Json;
using System.Collection.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

#region type aliases
$forall (n, t) <- types
  using ${n} = ${t}
#endregion

namespace ${namespace'}
{
    class ServantClient : HttpClient
    {
        public ServantClient()
        {
            this.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        }
    }

    public class API
    {
        #region fields
        private string server;
        #endregion

        #region properties
        #endregion

        #region Constructor
        public API(string _server)
        {
            this.server = _server;
        }
        #endregion

        #region APIs
        $forall ep <- getEndpoints
          public async Task<${retType ep}> ${methodName ep}Async(${paramDecl ep})
          {
              var client = new ServantClient();
              var queryparams = new List<string> {
                  $forall (_, qp) <- queryparams ep
                    _${qp}.HasValue ? $"_${qp}={_${qp}.Value}" : null,
              }.Where(e => !string.IsNullOrEmpty(e));
              var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
                #if DEBUG
                var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
                #else
                var jsonObj = JsonConvert.SerializeObject(_obj);
                #endif
              $if requestBodyExists ep
                var res = await client.${methodType ep}Async($"{server}${uri ep}{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
              $else
                var res = await client.${methodType ep}Async($"{server}${uri ep}{qp}");
              Debug.WriteLine($">>> {res.RequestMessage}");
              $if requestBodyExists ep
                Debug.WriteLine($"-----");
                Debug.WriteLine(jsonObj);
                Debug.WriteLine($"-----");
              Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
              var content = await res.Content.ReadAsStringAsync();
              Debug.WriteLine($"<<< {content}");
              return JsonConvert.DeserializeObject<${retType ep}>(content);
         }
          public ${retType ep} ${methodName ep}(${paramDecl ep})
          {
              Task<${retType ep}> t = ${methodName ep}Async(${paramArg ep});
              return t.GetAwaiter().GetResult();
          }
        #endregion
    }
}|]
