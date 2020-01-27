module Main where


import Network.Wai.Handler.Warp

import Server (app)

import Database.PostgreSQL.Query

import Data.Pool
import Database.PostgreSQL.Simple (close)

import Web.ClientSession



main :: IO ()
main = do

  run 3001 (app "/Users/V3/windev/carrefour/docserver/static/docs")

{-



main4 :: IO ()
main4 = runTLS tlsOpts warpOpts appCookie
  where tlsOpts = tlsSettings "cert.pem" "key.pem"
        warpOpts = setPort 3001 defaultSettings

            -- "public.pem" "private.pem"

-}

