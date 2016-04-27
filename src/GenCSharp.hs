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

-- type Swag a = Swagger -> a
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

convProperty :: ParamName -> Referenced Schema -> Bool -> Swag FieldType
convProperty pname rs req
    = if req
      then convProp rs
      else return . FNullable =<< convProp rs
    where
      convProp :: Referenced Schema -> Swag FieldType
      convProp (Ref (Reference s)) = convRef s
      convProp (Inline s) = convert (pname, s)

convRef :: ParamName -> Swag FieldType
convRef pname = undefined
{-
convRef :: ParamName -> Swag FieldType
convRef pname
    = Swag $ \sw -> do
        maybe (notEnum sw) (pure $ FRefEnum pname) $ lookup pname (enums sw)
    where
      notEnum :: Swag FieldType
      notEnum sw = maybe (notPrim sw) (pure $ FRefPrim pname) $ lookup pname $ prims sw
      notPrim :: Swag FieldType
      notPrim sw = maybe (error "not found the reference") (pure $ FRefObject pname) $ lookup pname $ models sw
-}
  
convObject :: (Text, Schema) -> Swag FieldType
convObject (name, s) = undefined
{-
convObject (name, s) = FObject name <$> fields
    where
      fields :: Swag [(ParamName, FieldType)]
      fields sw = map (\(p, s) -> (p, convProperty p s (isReq p) sw)) props
      props :: [(ParamName, Referenced Schema)]
      props = M.toList $ s^.properties
      isReq pname = pname `elem` reqs
      reqs :: [ParamName]
      reqs = s^.required
-}

convert :: (Text, Schema) -> Swag FieldType
convert (name, s) = do
  if not $ null enums'
  then return $ FEnum name enums'
  else case type' of
         SwaggerString -> maybe (return FString) convByFormat format'
         SwaggerInteger -> return FInteger
         SwaggerNumber -> return FNumber
         SwaggerBoolean -> return FBool
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
      convByFormat :: Text -> Swag FieldType
      convByFormat "date" = return FDay
      convByFormat "yyyy-mm-ddThh:MM:ssZ" = return FUTCTime
      convByItemType :: SwaggerItems Schema -> Swag FieldType
      convByItemType (SwaggerItemsObject (Ref (Reference s))) = convRef s
      convByItemType (SwaggerItemsPrimitive _ _)
          = error "don't support SwaggerItemsPrimitive yet"
      convByItemType (SwaggerItemsArray _)
          = error "don't support SwaggerItemsArray yet"

enums :: Swag [(Text, FieldType)]
enums = do
  fns <- mapM (return.fst) =<< defs
  fts <- mapM convert =<< defs
  return $ filter (isFEnum . snd) $ zip fns fts
    where
      isFEnum (FEnum _ _) = True
      isFEnum _ = False

prims :: Swag [(Text, FieldType)]
prims = do
  fns <- mapM (return.fst) =<< defs
  fts <- mapM convert =<< defs
  return $ filter (isPrim . snd) $ zip fns fts
    where
      isPrim FString = True
      isPrim FInteger = True
      isPrim FNumber = True
      isPrim FBool = True
      isPrim FDay = True
      isPrim FUTCTime = True
      isPrim _ = False

models :: Swag [(Text, FieldType)]
models = do
  fns <- mapM (return.fst) =<< defs
  fts <- mapM convert =<< defs
  return $ filter (isObj . snd) $ zip fns fts
    where
      isObj (FObject _ _) = True
      isObj _ = False

enumCs :: Swag String
enumCs = undefined
{--
enumCs swagger = [heredoc|/* generated by servant-csharp */
namespace ServantClientBook
{
    $forall FEnum name cs <- map snd (enums swagger)
      #region ${T.unpack name}
      public enum ${T.unpack name}
      {
          $forall String c <- cs
            ${T.unpack c},
      }
      #endregion
}
|]
--}

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
classCs = undefined
{-
classCs sw = [heredoc|/* generated by servant-csharp */
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using System;
using System.Collections.Generic;

#region type alias
$forall (n, t) <- prims sw
  using ${T.unpack n} = ${showCSharpOriginalType t};
#endregion

namespace ${namespace conf}
{
    $forall (_, FObject name' fields) <- models sw
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
--}

