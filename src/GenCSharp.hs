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
               | FGeneral Text
               | FList FieldType
               | FNullable FieldType
                 deriving Show

convert :: (Text, Schema) -> FieldType
convert (name, s)
    = if null enums
      then case s^.type_ of
             SwaggerString -> maybe FString convByFormat $ s^.format
             SwaggerInteger -> FInteger
             SwaggerNumber -> FNumber
             SwaggerBoolean -> FBool
             SwaggerArray -> error "convert don't support yet SwaggerArray"
             SwaggerNull -> error "convert don't support yet SwaggerNull"
             SwaggerObject -> FGeneral name
      else FEnum name enums
    where
      enums = s^.paramSchema.enum_._Just
      convByFormat "date" = FDay
      convByFormat "yyyy-mm-ddThh:MM:ssZ" = FUTCTime
-- map (id.fst &&& convert) defs

enums :: [(Text, FieldType)]
enums = filter (isFEnum . snd) $ map (id.fst &&& convert) defs
    where
      isFEnum (FEnum _ _) = True
      isFEnum _ = False

prims :: [(Text, FieldType)]
prims = filter (isPrim . snd) $ map (id.fst &&& convert) defs
    where
      isPrim FString = True
      isPrim FInteger = True
      isPrim FNumber = True
      isPrim FBool = True
      isPrim _ = False
