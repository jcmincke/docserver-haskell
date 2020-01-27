{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RecordWildCards            #-}


module Server where

import Prelude

import Debug.Trace (trace)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control
import Control.Monad

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Maybe (catMaybes, Maybe(..))

import Data.Aeson as A
--import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.Pool
import Database.PostgreSQL.Simple (Connection)

import Network.Wai

import System.IO (FilePath)

import Servant
--import Servant.API (addHeader)
import Servant.Server.Experimental.Auth (AuthHandler)
import Web.ClientSession
import System.Directory as D

import Text.Regex.Posix

--import UVox.Handlers.Bundle as B
--import UVox.Handlers.Login as L
--import UVox.Handlers.Register as R
--import UVox.Handlers.Static as S


--import Auth

import System.FilePath.Posix



data Entry = MkEntry
  { name :: T.Text
  , versions :: [T.Text]
  }
  deriving (Eq, Show)


instance ToJSON Entry where
    toJSON (MkEntry {..}) =
      object  [ "name" .= name
              , "versions" .= versions
              ]


type EntryApi =
  "entries" :> Get '[JSON] [Entry]


handlerEntries dir = do
  fs <- liftIO $ listDirectory dir
  ds <- liftIO $ filterM (\f -> doesDirectoryExist (dir </> f)) fs
  let pairs = catMaybes $ map parse ds
  let dict = pack pairs
  let entities = map (\(n, vs) -> MkEntry n vs) $ M.toList dict
  return entities
  where
  parse :: String -> Maybe (String, String)
  parse dn =
    case dn =~ pat of
    [[_, n, v]] -> Just (n, v)
    _ -> Nothing

  pat = "([a-z]+)-([[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+)" :: String
  pack es =
    foldl go M.empty es
    where
    go acc (n, v) = M.insertWith (++) (T.pack n) [T.pack v] acc



type StaticAPI = "static" :> Raw


staticServer = serveDirectoryWebApp "static"



type DocServerAPI = EntryApi :<|> StaticAPI

docServerApi :: Proxy DocServerAPI
docServerApi = Proxy

docServer :: FilePath -> Server DocServerAPI
docServer dir = do
  handlerEntries dir
  :<|> staticServer

docServerContext :: Context ('[])
docServerContext = EmptyContext


app :: FilePath -> Application
app dir = do
  serveWithContext docServerApi (docServerContext) (docServer dir)







