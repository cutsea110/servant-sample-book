module Js where

import Servant.JS
import System.Directory (createDirectoryIfMissing)

import API (api)

main :: IO ()
main = do
  createDirectoryIfMissing True "js/vanilla"
  createDirectoryIfMissing True "js/jquery"
  createDirectoryIfMissing True "js/angular"
  createDirectoryIfMissing True "js/axios"

  writeJSForAPI api vanillaJS "js/vanilla/api.js"
  writeJSForAPI api jquery "js/jquery/api.js"
  writeJSForAPI api (angular defAngularOptions) "js/angular/api.js"
  writeJSForAPI api (axios defAxiosOptions) "js/axios/api.js"
