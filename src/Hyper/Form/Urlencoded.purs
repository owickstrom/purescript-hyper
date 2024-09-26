-- | Parser for the `application/x-www-form-urlencoded` format, commonly used
-- | for query strings and POST request bodies.
module Hyper.Form.Urlencoded
       ( parseUrlencoded
       ) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Data.String (split, joinWith, Pattern(Pattern))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import JSURI (decodeURIComponent)

toTuple :: Array String -> Either String (Tuple String (Maybe String))
toTuple =
  case _ of
    [key] -> do
      key' <- decodeURIComponent' key
      pure (Tuple key' Nothing)
    [key, value] -> do
      key' <- decodeURIComponent' key
      value' <- decodeURIComponent' value
      pure (Tuple key' (Just value'))
    parts ->
      throwError ("Invalid form key-value pair: " <> joinWith " " parts)
  where
    decodeURIComponent' key = decodeURIComponent key # note ("Cannot decode URI component: " <> key)


parseUrlencoded :: String → Either String (Array (Tuple String (Maybe String)))
parseUrlencoded = split (Pattern "&")
                  >>> Array.filter (_ /= "")
                  >>> map (split (Pattern "="))
                  >>> map toTuple
                  >>> sequence
