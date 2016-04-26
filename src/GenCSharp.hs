{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module GenCSharp where

import Control.Arrow ((***), (&&&))
import Control.Lens
import Data.Aeson
import qualified Data.HashMap.Lazy as M
import Data.Swagger
import Data.Text as T (Text, unpack)
import System.Directory (createDirectoryIfMissing)
import Servant.Swagger

import Swagger
import API (api)

swagger :: Swagger
swagger = toSwagger api

defs :: [(Text, Schema)]
defs = concatMap M.toList $ swagger^..definitions

pathitems :: [(FilePath, PathItem)]
pathitems = concatMap M.toList $ swagger^..paths

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

convProperty :: ParamName -> Referenced Schema -> Bool -> FieldType
convProperty pname rs req = if req
                            then convProp rs
                            else FNullable $ convProp rs
    where
      convProp :: Referenced Schema -> FieldType
      convProp (Ref (Reference s)) = convRef s
      convProp (Inline s) = convert (pname, s)

convRef :: ParamName -> FieldType
convRef pname
    = maybe notEnum (const $ FRefEnum pname) $ lookup pname enums
    where
      notEnum = maybe notPrim (FRefPrim pname) $ lookup pname prims
      notPrim = maybe (error "not found the reference") (const $ FRefObject pname) $ lookup pname models

        

convObject :: (Text, Schema) -> FieldType
convObject (name, s) = FObject name fields
    where
      fields :: [(ParamName, FieldType)]
      fields = map (\(p, s) -> (p, convProperty p s $ isReq p)) props
      props :: [(ParamName, Referenced Schema)]
      props = M.toList $ s^.properties
      isReq pname = pname `elem` reqs
      reqs :: [ParamName]
      reqs = s^.required

convert :: (Text, Schema) -> FieldType
convert (name, s)
    = if not $ null enums
      then FEnum name enums
      else case s^.type_ of
             SwaggerString -> maybe FString convByFormat $ s^.format
             SwaggerInteger -> FInteger
             SwaggerNumber -> FNumber
             SwaggerBoolean -> FBool
             SwaggerArray -> FList itemType
             SwaggerNull -> error "convert don't support yet SwaggerNull"
             SwaggerObject -> convObject (name, s)
    where
      enums = s^.paramSchema.enum_._Just
      convByFormat "date" = FDay
      convByFormat "yyyy-mm-ddThh:MM:ssZ" = FUTCTime
      props = M.toList $ s^.properties
      itemType = maybe (error "missing SwaggaerItemsObject") convByItems
                 $ s^.items
      convByItems :: SwaggerItems Schema -> FieldType
      convByItems (SwaggerItemsPrimitive _ _)
          = error "don't support SwaggerItemsPrimitive"
      convByItems (SwaggerItemsObject (Ref (Reference s))) = convRef s
      convByItems (SwaggerItemsArray _)
          = error "don't support SwaggerItemsArray"
-- map (fst &&& convert) defs

enums :: [(Text, FieldType)]
enums = filter (isFEnum . snd) $ map (fst &&& convert) defs
    where
      isFEnum (FEnum _ _) = True
      isFEnum _ = False

prims :: [(Text, FieldType)]
prims = filter (isPrim . snd) $ map (fst &&& convert) defs
    where
      isPrim FString = True
      isPrim FInteger = True
      isPrim FNumber = True
      isPrim FBool = True
      isPrim FDay = True
      isPrim FUTCTime = True
      isPrim _ = False

models :: [(Text, FieldType)]
models = filter (isObj . snd) $ map (fst &&& convert) defs
    where
      isObj (FObject _ _) = True
      isObj _ = False
