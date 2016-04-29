module GenCSharp where

import CS.JsonDotNet (generateCsForAPI, def, GenerateCsConfig(..))

import Swagger hiding (main)
import API (api)

main :: IO ()
main = generateCsForAPI def { namespace = "ServantClientBook" } api
