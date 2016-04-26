module GenCSharp where

import Control.Lens
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

prims :: [(Text, Schema)]
prims = filter (\def -> prim def && notEnum def) defs
    where
      prim = (`elem`primTypes).(^.paramSchema.type_).snd
      notEnum = (`notElem`enumTypes).fst
      enumTypes = map fst enums
      primTypes = [ SwaggerString
                  , SwaggerInteger
                  , SwaggerNumber
                  , SwaggerBoolean]
