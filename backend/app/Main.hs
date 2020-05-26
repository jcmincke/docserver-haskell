module Main where


import Network.Wai.Handler.Warp

import Server (app)


import Web.ClientSession



main :: IO ()
main = do
  run 3001 (app "./static/docs")


