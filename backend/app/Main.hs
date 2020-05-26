module Main where


import Network.Wai.Handler.Warp

import Server (app)

import System.Environment (getArgs)
import Web.ClientSession



main :: IO ()
main = do
  [path_docs] <- getArgs
  run 3001 (app path_docs)


