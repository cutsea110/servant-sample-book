{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module GenCSharp where

import Control.Arrow ((***), (&&&))
import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.HashMap.Lazy as M
import Data.Monoid ((<>))
import Data.Proxy
import Data.Swagger hiding (namespace)
import Data.Text as T (Text, unpack)
import System.Directory (createDirectoryIfMissing)
import Servant.Swagger
import Text.Heredoc

import Swagger
import API (api)

newtype Swag a = Swag { runSwagger :: Swagger -> a }
instance Functor Swag where
    fmap f (Swag g) = Swag (f . g)
instance Applicative Swag where
    pure = Swag . const
    (Swag f) <*> (Swag g) = Swag (f <*> g)
instance Monad Swag where
    (Swag f) >>= k = Swag $ \sw -> runSwagger (k (f sw)) sw

instance Monoid a => Monoid (Swag a) where
    mempty = Swag mempty
    (Swag x) `mappend` (Swag y) = Swag (x `mappend` y)

defs :: Swag [(Text, Schema)]
defs = Swag $ \sw -> concatMap M.toList $ sw^..definitions

pathitems :: Swag [(FilePath, PathItem)]
pathitems = Swag $ \sw -> concatMap M.toList $ sw^..paths

data FieldType = FInteger
               | FNumber
               | FString
               | FBool
               | FDay
               | FUTCTime
               | FEnum Text [Value]
               | FObject Text [(Text, FieldType)]
               | FList FieldType
               | FNullable FieldType

               | FRefObject Text
               | FRefEnum Text
               | FRefPrim Text FieldType
                 deriving Show

convProperty :: ParamName -> Referenced Schema -> Bool -> Swag (ParamName, FieldType)
convProperty pname rs req
    = if req
      then convProp pname rs
      else do
        (n, f) <- convProp pname rs
        return (n, FNullable f)
    where
      convProp :: ParamName -> Referenced Schema -> Swag (ParamName, FieldType)
      convProp n (Ref (Reference s)) = convRef n s
      convProp n (Inline s) = convert (n, s)

convRef :: ParamName -> Text -> Swag (ParamName, FieldType)
convRef pname tname = do
  fs <- enums <> prims <> models
  case lookup tname fs of
    Just ftype -> return $ (pname, conv ftype)
    Nothing -> error $ T.unpack $ "not found " <> pname
  where
    conv :: FieldType -> FieldType
    conv f | isFEnum f = FRefEnum tname
           | isFPrim f = FRefPrim tname f
           | isFObj  f = FRefObject tname
  
convObject :: (Text, Schema) -> Swag (Text, FieldType)
convObject (name, s) = do
  return . (name,) . FObject name =<< fields
    where
      fields :: Swag [(ParamName, FieldType)]
      fields = mapM (\(p, s) -> (convProperty p s (isReq p))) props
      props :: [(ParamName, Referenced Schema)]
      props = M.toList $ s^.properties
      isReq :: ParamName -> Bool
      isReq pname = pname `elem` reqs
      reqs :: [ParamName]
      reqs = s^.required

convert :: (Text, Schema) -> Swag (Text, FieldType)
convert (name, s) = do
  if not $ null enums'
  then return $ (name, FEnum name enums')
  else case type' of
         SwaggerString -> maybe (return (name, FString)) convByFormat format'
         SwaggerInteger -> return (name, FInteger)
         SwaggerNumber -> return (name, FNumber)
         SwaggerBoolean -> return (name, FBool)
         SwaggerArray -> maybe (error "fail to convert SwaggerArray")
                               convByItemType
                               items'
         SwaggerNull -> error "convert don't support SwaggerNull yet"
         SwaggerObject -> convObject (name, s)
    where
      items' = s^.items
      type' = s^.type_
      enums' = s^.paramSchema.enum_._Just
      format' = s^.format
      convByFormat :: Text -> Swag (Text, FieldType)
      convByFormat "date" = return (name, FDay)
      convByFormat "yyyy-mm-ddThh:MM:ssZ" = return (name, FUTCTime)
      convByItemType :: SwaggerItems Schema -> Swag (Text, FieldType)
      convByItemType (SwaggerItemsObject (Ref (Reference s))) = do
                      (n, t) <- convRef name s
                      return (n, FList t)
      convByItemType (SwaggerItemsPrimitive _ _)
          = error "don't support SwaggerItemsPrimitive yet"
      convByItemType (SwaggerItemsArray _)
          = error "don't support SwaggerItemsArray yet"

enums :: Swag [(Text, FieldType)]
enums = return . filter (isFEnum . snd) =<< mapM convert =<< defs

isFEnum (FEnum _ _) = True
isFEnum _ = False

prims :: Swag [(Text, FieldType)]
prims = return . filter (isFPrim . snd) =<< mapM convert =<< defs

isFPrim FString = True
isFPrim FInteger = True
isFPrim FNumber = True
isFPrim FBool = True
isFPrim FDay = True
isFPrim FUTCTime = True
isFPrim _ = False

models :: Swag [(Text, FieldType)]
models = return . filter (isFObj . snd) =<< mapM convert =<< defs

isFObj (FObject _ _) = True
isFObj _ = False

enumCs :: Swag String
enumCs = do
  es <- mapM (return.snd) =<< enums
  return [heredoc|/* generated by servant-csharp */
namespace ServantClientBook
{
    $forall FEnum name cs <- es
      #region ${T.unpack name}
      public enum ${T.unpack name}
      {
          $forall String c <- cs
            ${T.unpack c},
      }
      #endregion
}
|]

showCSharpOriginalType :: FieldType -> String
showCSharpOriginalType FInteger = "System.Int64"
showCSharpOriginalType FNumber = "System.Double"
showCSharpOriginalType FString = "System.String"
showCSharpOriginalType FDay = "System.DateTime"
showCSharpOriginalType FUTCTime = "System.DateTime"
showCSharpOriginalType _ = error "don't support this type."

data CCate = CVal | CRef | CSt deriving Show

nullable :: FieldType -> CCate
nullable FInteger = CVal
nullable FNumber = CVal
nullable FString = CRef
nullable FBool = CVal
nullable FDay = CVal
nullable FUTCTime = CVal
nullable (FEnum _ _) = CSt
nullable (FObject _ _) = CSt
nullable (FList _) = CRef
nullable (FNullable _) = CRef
nullable (FRefObject _) = CVal
nullable (FRefEnum _) = CVal
nullable (FRefPrim _ t) = nullable t

show' :: FieldType -> String
show' FInteger = "int"
show' FNumber = "double"
show' FString = "string"
show' FBool = "bool"
show' FDay = "DateTime"
show' FUTCTime = "DateTime"
show' (FEnum name _) = T.unpack name
show' (FObject name _) = T.unpack name
show' (FList t) = "List<" <> show' t <> ">"
show' (FNullable t) = case nullable t of
                        CVal -> show' t <> "?"
                        CRef -> show' t
                        CSt  -> "Nullable<" <> show' t <> ">"
show' (FRefObject name) = T.unpack name
show' (FRefEnum name) = T.unpack name
show' (FRefPrim name _) = T.unpack name

data ConverterType = NoConv
                   | DayConv
                   | EnumConv
                   | ItemConv ConverterType
                     deriving Show

converterType :: FieldType -> ConverterType
converterType FDay = DayConv
converterType (FRefPrim _ FDay) = DayConv
converterType (FEnum _ _) = EnumConv
converterType (FRefEnum _) = EnumConv
converterType (FNullable t) = converterType t
converterType (FList t) = case converterType t of
                            DayConv -> ItemConv DayConv
                            EnumConv -> ItemConv EnumConv
                            t' -> t'
converterType _ = NoConv

data Config = Config { namespace :: String
                     } deriving Show

conf :: Config
conf = Config { namespace = "ServantClientBook" }

classCs :: Swag String
classCs = do
  ps <- prims
  ms <- models
  return [heredoc|/* generated by servant-csharp */
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using System;
using System.Collections.Generic;

#region type alias
$forall (n, t) <- ps
  using ${T.unpack n} = ${showCSharpOriginalType t};
#endregion

namespace ${namespace conf}
{
    $forall (_, FObject name' fields) <- ms
      $let name = T.unpack name'
        #region ${name}
        [JsonObject("${name}")]
        public class ${name}
        {
            $forall (fname', ftype) <- fields
              $let fname = T.unpack fname'
                $case converterType ftype
                  $of DayConv
                    [JsonProperty(PropertyName = "${fname}")]
                    [JsonConverter(typeof(DayConverter))]
                  $of ItemConv DayConv
                    [JsonProperty(PropertyName = "${fname}", ItemConverterType = typeof(DayConverter))]
                  $of EnumConv
                    [JsonProperty(PropertyName = "${fname}")]
                    [JsonConverter(typeof(StringEnumConverter))]
                  $of ItemConv EnumConv
                    [JsonProperty(PropertyName = "${fname}", ItemConverterType = typeof(StringEnumConverter))]
                  $of _
                    [JsonProperty(PropertyName = "${fname}")]
                public ${show' ftype} ${fname} { get; set; }
        }
        #endregion
}
|]
