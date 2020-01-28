module Main where


import Network.Wai.Handler.Warp

import Server (app)

import Database.PostgreSQL.Query

import Data.Pool
import Database.PostgreSQL.Simple (close)

import Web.ClientSession



main :: IO ()
main = do
  run 3001 (app "./static/docs")


