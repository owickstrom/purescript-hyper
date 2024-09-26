module Hyper.Node.Server.Options
       ( defaultOptions
       , defaultOptionsWithLogging
       , Hostname(..)
       , Options(..)
       , Port(..)
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.Net.Types (IpFamily)

newtype Hostname = Hostname String
derive instance newtypeHostname :: Newtype Hostname _

newtype Port = Port Int
derive instance newtypePort :: Newtype Port _

type Options =
  { hostname :: Hostname
  , port :: Port
  , onListening :: Maybe { port :: Int, family :: IpFamily, address :: String } -> Effect Unit
  , onRequestError :: Error -> Effect Unit
  }


defaultOptions :: Options
defaultOptions =
  { hostname: Hostname "0.0.0.0"
  , port: Port 0 -- use random port
  , onListening: const (pure unit)
  , onRequestError: const (pure unit)
  }


defaultOptionsWithLogging :: Options
defaultOptionsWithLogging =
  defaultOptions { onListening = onListening
                 , onRequestError = onRequestError
                 }
  where
    onListening =
      case _ of
        Nothing -> log ("Something wrong: server initialized but is not yet started")
        Just addressOrSocket -> log ("Listening on http://" <> addressOrSocket.address <> ":" <> show addressOrSocket.port)

    onRequestError err =
      log ("Request failed: " <> show err)

