module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)

import API (api, server)

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app
