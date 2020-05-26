module Main where


import Network.Wai.Handler.Warp

import Server (app)

import System.Environment (getArgs)
import Web.ClientSession



main :: IO ()
main = do
  args <- getArgs
  let [port, path_docs] =
        case args of
          [port] -> [port, "./static/docs"]
          [_, _] -> args
  putStrLn path_docs
  putStrLn port
  run (read port::Int) (app path_docs)


