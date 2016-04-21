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
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text as T (Text, unpack, pack)
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

-- | TODO : rename
namespace :: String
namespace = "ServantClientBook"

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
paramDecl req = intercalate ", " $ map help $ paramInfos req
    where
      help :: (String, String) -> String
      help (t, n) = t ++ " " ++ ("_"++n)

paramArg :: Req Text -> String
paramArg req = intercalate ", " $ map fst $ paramInfos req

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

namespace ${namespace}
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

main :: IO ()
main = do
  createDirectoryIfMissing True "gen/ServantClientBook/ServantClientBook"
  writeFile "gen/ServantClientBook/ServantClientBook/API.cs" template
