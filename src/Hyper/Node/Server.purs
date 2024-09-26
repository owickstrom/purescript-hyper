module Hyper.Node.Server
       ( HttpRequest(..)
       , HttpResponse(..)
       , NodeResponse(..)
       , writeString
       , write
       , module Hyper.Node.Server.Options
       , runServer
       , runServer'
       , getWriter
       ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Indexed (ipure, (:>>=))
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..), either)
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Lazy (defer)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff, launchAff_, makeAff, nonCanceler, runAff_)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (catchException)
import Foreign.Object as Object
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, evalMiddleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Node.Server.Options (Hostname(..), Options, Port(..), defaultOptions, defaultOptionsWithLogging) as Hyper.Node.Server.Options
import Hyper.Node.Server.Options (Options)
import Hyper.Request (class ReadableBody, class Request, class StreamableBody, RequestData, parseUrl, readBody)
import Hyper.Response (class ResponseWritable, class Response, ResponseEnded, StatusLineOpen)
import Hyper.Status (Status(..))
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.EventEmitter as EventEmitter
import Node.HTTP as HTTP
import Node.HTTP.IncomingMessage as HTTP.IncomingMessage
import Node.HTTP.OutgoingMessage as HTTP.OutgoingMessage
import Node.HTTP.Server as HTTP.Server
import Node.HTTP.ServerResponse as HTTP.ServerResponse
import Node.HTTP.Types (IncomingMessage)
import Node.HTTP.Types as HTTP
import Node.Net.Server as Net.Server
import Node.Net.Types (Server)
import Node.Stream (Stream, Writable, Readable)
import Node.Stream as Stream


data HttpRequest
  = HttpRequest (HTTP.IncomingMessage HTTP.IMServer) RequestData


instance requestHttpRequest :: Monad m => Request HttpRequest m where
  getRequestData = do
    getConn :>>=
      case _ of
        { request: HttpRequest _ d } -> ipure d


-- A limited version of Writable () e, with which you can only write, not end,
-- the Stream.
newtype NodeResponse m
  = NodeResponse (Writable () -> m Unit)

writeString :: forall m. MonadAff m => Encoding -> String -> NodeResponse m
writeString enc str = NodeResponse $ \w ->
  liftAff (makeAff (\k -> Stream.writeString' w enc str (\err -> k (maybe (Right unit) Left err)) *> pure nonCanceler))

write :: forall m. MonadAff m => Buffer -> NodeResponse m
write buffer = NodeResponse $ \w ->
  liftAff (makeAff (\k -> Stream.write' w buffer (\err -> k (maybe (Right unit) Left err)) *> pure nonCanceler))

instance stringNodeResponse :: MonadAff m => ResponseWritable (NodeResponse m) m String where
  toResponse = ipure <<< writeString UTF8

instance stringAndEncodingNodeResponse :: MonadAff m => ResponseWritable (NodeResponse m) m (Tuple String Encoding) where
  toResponse (Tuple body encoding) =
    ipure (writeString encoding body)

instance bufferNodeResponse :: MonadAff m
                                  => ResponseWritable (NodeResponse m) m Buffer where
  toResponse buf =
    ipure (write buf)

-- Helper function that reads a Stream into a Buffer, and throws error
-- in `Aff` when failed.
readBodyAsBuffer
  :: HttpRequest
  -> Aff Buffer
readBodyAsBuffer (HttpRequest incomingMessage _) = do
  let stream = HTTP.IncomingMessage.toReadable incomingMessage
  bodyResult <- AVar.empty
  chunks <- AVar.new []
  fillResult <- liftEffect $
    catchException (pure <<< Left) (Right <$> fillBody stream chunks bodyResult)
  -- Await the body, or an error.
  body <- AVar.take bodyResult
  -- Return the body, if neither `fillResult` nor `body` is a `Left`.
  either throwError pure (fillResult *> body)
  where
    fillBody stream chunks bodyResult = do
      -- Append all chunks to the body buffer.
      stream # EventEmitter.on_ Stream.dataH \chunk ->
        let modification = do
              v <- AVar.take chunks
              AVar.put (v <> [chunk]) chunks
        in void (launchAff modification)
      -- Complete with `Left` on error.
      stream # EventEmitter.on_ Stream.errorH (\error -> launchAff_ $ flip AVar.put bodyResult $ Left error)
      -- Complete with `Right` on successful "end" event.
      stream # EventEmitter.on_ Stream.endH (
        void $ launchAff $
          AVar.take chunks
          >>= concat'
          >>= (pure <<< Right)
          >>= flip AVar.put bodyResult
      )
    concat' = liftEffect <<< Buffer.concat

instance readableBodyHttpRequestString :: (Monad m, MonadAff m)
                                       => ReadableBody HttpRequest m String where
  readBody =
    readBody :>>= (\(buffer :: Buffer) -> liftEffect $ Buffer.toString UTF8 buffer)

instance readableBodyHttpRequestBuffer :: (Monad m, MonadAff m)
                                       => ReadableBody HttpRequest m Buffer where
  readBody =
    _.request <$> getConn :>>=
    case _ of
      r -> liftAff (readBodyAsBuffer r)

instance streamableBodyHttpRequestReadable :: MonadAff m
                                           => StreamableBody
                                              HttpRequest
                                              m
                                              (Readable ()) where
  streamBody =
    _.request <$> getConn :>>=
    case _ of
      HttpRequest incomingMessage _ -> ipure (HTTP.IncomingMessage.toReadable incomingMessage)

newtype HttpResponse state = HttpResponse HTTP.ServerResponse

getWriter :: forall req res c m rw.
            Monad m =>
            Middleware
            m
            (Conn req { writer :: rw | res } c)
            (Conn req { writer :: rw | res } c)
            rw
getWriter = _.response.writer <$> getConn

setStatus :: forall req res c m.
            MonadEffect m
          => Status
          -> HTTP.ServerResponse
          -> Middleware m (Conn req res c) (Conn req res c) Unit
setStatus (Status { code, reasonPhrase }) r = liftEffect do
  HTTP.ServerResponse.setStatusCode code r
  HTTP.ServerResponse.setStatusMessage reasonPhrase r

writeHeader' :: forall req res c m.
               MonadEffect m
             => (Tuple String String)
             -> HTTP.ServerResponse
             -> Middleware m (Conn req res c) (Conn req res c) Unit
writeHeader' (Tuple name value) serverResponse =
  liftEffect $ HTTP.OutgoingMessage.setHeader name value (HTTP.ServerResponse.toOutgoingMessage serverResponse)

writeResponse :: forall req res c m.
                MonadAff m
             => HTTP.ServerResponse
             -> NodeResponse m
             -> Middleware m (Conn req res c) (Conn req res c) Unit
writeResponse r (NodeResponse f) =
  lift' $ f $ HTTP.OutgoingMessage.toWriteable $ HTTP.ServerResponse.toOutgoingMessage r

endResponse :: forall req res c m.
              MonadEffect m
            => HTTP.ServerResponse
            -> Middleware m (Conn req res c) (Conn req res c) Unit
endResponse r =
  liftEffect (Stream.end (HTTP.OutgoingMessage.toWriteable $ HTTP.ServerResponse.toOutgoingMessage r))

instance responseWriterHttpResponse :: MonadAff m
                                    => Response HttpResponse m (NodeResponse m) where
  writeStatus status = Ix.do
    { response: HttpResponse r } <- getConn
    setStatus status r
    modifyConn (_ { response = HttpResponse r })

  writeHeader header = Ix.do
    { response: HttpResponse r } <- getConn
    writeHeader' header r
    modifyConn (_ { response = HttpResponse r })

  closeHeaders = Ix.do
    { response: HttpResponse r } <- getConn
    modifyConn (_ { response = HttpResponse r })

  send f = Ix.do
    { response: HttpResponse r } <- getConn
    writeResponse r f
    modifyConn (_ { response = HttpResponse r })

  end = Ix.do
    { response: HttpResponse r } <- getConn
    endResponse r
    modifyConn (_ { response = HttpResponse r })


mkHttpRequest :: HTTP.IncomingMessage HTTP.IMServer -> HttpRequest
mkHttpRequest request =
  let
    headers = HTTP.IncomingMessage.headers request
    requestData =
      { url: HTTP.IncomingMessage.url request
      , parsedUrl: defer \_ -> parseUrl (HTTP.IncomingMessage.url request)
      , headers: headers
      , method: Method.fromString (HTTP.IncomingMessage.method request)
      , contentLength: Object.lookup "content-length" headers >>= Int.fromString
      }
  in HttpRequest request requestData


runServer'
  :: forall m c c'
   . Functor m
  => Options
  -> c
  -> (forall a. m a -> Aff a)
  -> Middleware
     m
     (Conn HttpRequest (HttpResponse StatusLineOpen) c)
     (Conn HttpRequest (HttpResponse ResponseEnded) c')
     Unit
  -> Effect Unit
runServer' options components runM middleware = do
  server <- HTTP.createServer
  server # EventEmitter.on_ HTTP.Server.requestH onRequest
  HTTP.Server.toNetServer server # EventEmitter.once_ Net.Server.listeningH (listenCallback server)
  Net.Server.listenTcp (HTTP.Server.toNetServer server) { port: unwrap options.port, host: unwrap options.hostname }
  where
    listenCallback :: HTTP.HttpServer -> Effect Unit
    listenCallback server = Net.Server.addressTcp (HTTP.Server.toNetServer server) >>= options.onListening

    onRequest :: HTTP.IncomingMessage HTTP.IMServer -> HTTP.ServerResponse -> Effect Unit
    onRequest request response =
      let conn :: { components :: c, request :: HttpRequest, response :: HttpResponse StatusLineOpen }
          conn = { request: mkHttpRequest request
                 , response: HttpResponse response
                 , components: components
                 }
          callback :: Either Error { components :: c', request :: HttpRequest, response :: HttpResponse ResponseEnded } → Effect Unit
          callback =
            case _ of
              Left err -> options.onRequestError err
              Right _ -> pure unit
      in conn
         # evalMiddleware middleware
         # runM
         # runAff_ callback

runServer
  :: forall c c'.
     Options
  -> c
  -> Middleware
     Aff
     (Conn HttpRequest (HttpResponse StatusLineOpen) c)
     (Conn HttpRequest (HttpResponse ResponseEnded) c')
     Unit
  -> Effect Unit
runServer options components middleware =
  runServer' options components identity middleware
