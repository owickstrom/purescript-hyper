module FormSerialization where

import Prelude
import Data.Int as Int
import Control.Monad.Indexed ((:>>=), (:*>))
import Effect (Effect)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Maybe (maybe)
import Hyper.Conn (Conn)
import Hyper.Form (class FromForm, parseFromForm, required)
import Hyper.Middleware (Middleware)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Request (class ReadableBody, class Request, getRequestData)
import Hyper.Response (class Response, class ResponseWritable, ResponseEnded, StatusLineOpen, closeHeaders, respond, writeStatus)
import Hyper.Status (statusBadRequest, statusMethodNotAllowed)

-- start snippet datatypes
data MealType = Vegan | Vegetarian | Omnivore | Carnivore

derive instance genericMealType :: Generic MealType _
instance eqMealType :: Eq MealType where eq = genericEq
instance showMealType :: Show MealType where show = genericShow

newtype Order = Order { beers :: Int, meal :: MealType }
-- end snippet datatypes

-- start snippet parsing
instance fromFormOrder :: FromForm Order where
  fromForm form = do
    beers <- required "beers" form >>= parseBeers
    meal <- required "meal" form >>= parseMealType
    pure (Order { beers: beers, meal: meal })
    where
      parseBeers s =
        maybe
        (throwError ("Invalid number: " <> s))
        pure
        (Int.fromString s)

      parseMealType =
        case _ of
          "Vegan" -> pure Vegan
          "Vegetarian" -> pure Vegetarian
          "Omnivore" -> pure Omnivore
          "Carnivore" -> pure Carnivore
          s -> throwError ("Invalid meal type: " <> s)
-- end snippet parsing

onPost
  :: forall m b req res c
  .  Monad m
  => Request req m
  => ReadableBody req m String
  => Response res m b
  => ResponseWritable b m String
  => FromForm Order
  => Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
-- start snippet onPost
onPost =
  parseFromForm :>>=
  case _ of
    Left err ->
      writeStatus statusBadRequest
      :*> closeHeaders
      :*> respond (err <> "\n")
    Right (Order { beers, meal })
      | meal == Omnivore || meal == Carnivore ->
        writeStatus statusBadRequest
        :*> closeHeaders
        :*> respond "Sorry, we do not serve meat here.\n"
      | otherwise ->
        writeStatus statusBadRequest
        :*> closeHeaders
        :*> respond ("One " <> show meal <> " meal and "
                     <> show beers <> " beers coming up!\n")
-- end snippet onPost

main :: Effect Unit
main =
  let
    router =
      _.method <$> getRequestData :>>=
      case _ of
        Left POST -> onPost
        Left method ->
          writeStatus statusMethodNotAllowed
          :*> closeHeaders
          :*> respond ("Method not supported: " <> show method)
        Right customMethod ->
          writeStatus statusMethodNotAllowed
          :*> closeHeaders
          :*> respond ("Custom method not supported: " <> show customMethod)

  -- Let's run it.
  in runServer defaultOptionsWithLogging {} router
