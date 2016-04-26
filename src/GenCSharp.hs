{-# LANGUAGE GADTs #-}
module GenCSharp where

import Control.Arrow ((***), (&&&))
import Control.Lens
import Data.Aeson
import qualified Data.HashMap.Lazy as M
import Data.Swagger
import Data.Text (Text)
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

enums :: [(Text, Schema)]
enums = filter (has (paramSchema.enum_._Just) . snd) defs
-- map (id *** ((^.paramSchema.type_) &&& (^.paramSchema.enum_._Just))) enums

prims :: [(Text, Schema)]
prims = filter (\def -> prim def && notEnum def) defs
    where
      prim = (`elem`primTypes).(^.paramSchema.type_).snd
      notEnum = (`notElem`enumTypes).fst
      enumTypes = map fst enums
      primTypes = [ SwaggerString
                  , SwaggerInteger
                  , SwaggerNumber
                  , SwaggerBoolean
                  ]
-- map (id *** convert) prims

models :: [(Text, Schema)]
models = filter (\def -> notPrim def && notEnum def) defs
    where
      notEnum = (`notElem`enumTypes).fst
      enumTypes = map fst enums
      notPrim = (`notElem`primTypes).fst
      primTypes = map fst prims

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
    = if null $ s^.paramSchema.enum_._Just
      then case s^.type_ of
             SwaggerString -> FString
             SwaggerInteger -> FInteger
             SwaggerNumber -> FNumber
             SwaggerBoolean -> FBool
             SwaggerArray -> error "convert don't support yet SwaggerArray"
             SwaggerNull -> error "convert don't support yet SwaggerNull"
             SwaggerObject -> FGeneral name
      else FEnum name (s^.paramSchema.enum_._Just)
