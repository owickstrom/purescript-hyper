module Hyper.Cookies
       ( cookies
       , CookieAttributes
       , SameSite(..)
       , Name
       , maxAge
       , MaxAge
       , defaultCookieAttributes
       , setCookieHeaderValue
       , setCookie
       , Value
       , Values
       ) where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Error.Class (throwError)
import Data.Array (catMaybes, cons, filter, foldMap, uncons, (:))
import Data.Either (Either, note)
import Data.JSDate (JSDate, toUTCString)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(NonEmpty), (:|))
import Data.NonEmpty as NonEmpty
import Data.String (Pattern(..), joinWith, split, trim)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), uncurry)
import Foreign.Object (Object)
import Foreign.Object as Object
import JSURI (encodeURIComponent, decodeURIComponent)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)
import Hyper.Middleware.Class (getConn, putConn)
import Hyper.Request (class Request, getRequestData)
import Hyper.Response (class Response, HeadersOpen, writeHeader)

type Name = String
type Value = String
type Values = NonEmpty Array Value

toPair :: Array String -> Either String (Tuple String (Array String))
toPair =
  case _ of
    [key, value] -> do
      key' <- decodeURIComponent' key
      value' <- decodeURIComponent' value
      pure (Tuple key' [value'])
    parts ->
      throwError ("Invalid cookie-pair: " <> joinWith " " parts)
  where
    decodeURIComponent' key = decodeURIComponent key # note ("Cannot decode URI component: " <> key)

splitPairs :: String -> Either String (Array (Tuple String (Array String)))
splitPairs =
  split (Pattern ";")
  >>> map trim
  >>> filter ((/=) "")
  >>> map (split (Pattern "="))
  >>> map toPair
  >>> sequence

parseCookies :: String -> Either String (Object Values)
parseCookies s =
  splitPairs s
  # map (foldMap toCookieMap)
  # map (Object.fromFoldableWith combineCookies)
  where
    toCookieMap (Tuple name values) =
      case uncons values of
        Just { head, tail } -> [Tuple name (NonEmpty head tail)]
        Nothing -> []

    combineCookies xs xs' =
      NonEmpty.head xs :| NonEmpty.head xs' : NonEmpty.tail xs <> NonEmpty.tail xs'

cookies :: forall m req res c
  .  Monad m
  => Request req m
  => Middleware
     m
     (Conn req res { cookies :: Unit | c})
     (Conn req res { cookies :: Either String (Object Values) | c})
     Unit
cookies = Ix.do
  conn <- getConn
  { headers } <- getRequestData
  let cookies' = maybe (pure Object.empty) parseCookies (Object.lookup "cookie" headers)
  putConn conn { components { cookies = cookies' }}

data SameSite = Strict | Lax
newtype MaxAge = MaxAge Int
derive instance newtypeMaxAge :: Newtype MaxAge _

maxAge :: Int -> Maybe MaxAge
maxAge a | a < 0 = Nothing
         | otherwise = Just (MaxAge a)

type CookieAttributes =
  { comment :: Maybe String
  , domain :: Maybe String
  , expires :: Maybe JSDate
  , httpOnly :: Boolean
  , maxAge :: Maybe MaxAge
  , path :: Maybe String
  , sameSite :: Maybe SameSite
  , secure :: Boolean
  }

defaultCookieAttributes :: CookieAttributes
defaultCookieAttributes =
  { comment: Nothing
  , domain: Nothing
  , expires: Nothing
  , httpOnly: false
  , maxAge: Nothing
  , path: Nothing
  , sameSite: Nothing
  , secure: false
  }

setCookieHeaderValue :: Name -> Value -> CookieAttributes -> String
setCookieHeaderValue = \key value { comment, expires, path, maxAge: m, domain, secure, httpOnly, sameSite } ->
  [ (Tuple "Comment" <<< unsafeEncodeURIComponent) <$> comment
  , (Tuple "Expires" <<< toUTCString) <$> expires
  , (Tuple "Max-Age" <<< show <<< unwrap) <$> m
  , Tuple "Domain" <$> domain
  , Tuple "Path" <$> path
  , Tuple "SameSite" <<< sameSiteSer <$> sameSite
  ]
  # map (uncurry assigment <$> _)
  # cons (if secure then Just "Secure" else Nothing)
  # cons (if httpOnly then Just "HttpOnly" else Nothing)
  # catMaybes
  # cons (assigment (unsafeEncodeURIComponent key)  (unsafeEncodeURIComponent value))
  # joinWith ";"
 where
  assigment k v = k <> "=" <> v

  unsafeEncodeURIComponent s =
    case encodeURIComponent s of
         Nothing -> unsafePartial $ crashWith "Hyper.Cookies.setCookieHeaderValue: Cannot encodeURIComponent " <> s
         Just s' -> s'

  sameSiteSer :: SameSite -> String
  sameSiteSer Strict = "Strict"
  sameSiteSer Lax = "Lax"

setCookie :: forall m req res c b
  .  Monad m
  => Response res m b
  => Name
  -> Value
  -> CookieAttributes
  -> Middleware
     m
     (Conn req (res HeadersOpen) c)
     (Conn req (res HeadersOpen) c)
     Unit
setCookie key value attrs =
  writeHeader (Tuple "Set-Cookie" (setCookieHeaderValue key value attrs))
