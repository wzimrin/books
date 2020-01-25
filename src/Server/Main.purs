module Server.Main where

import Prelude
import Control.Monad.Reader (runReaderT)
import Data.Map (empty)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Express.App (App, get, listenHttp)
import Node.Express.Handler (HandlerM, Handler)
import Node.Express.Response (send, sendJson)
import Server.Database (dbConn)
import Server.Handlers (Request, Response, getBookH)
import Shared.ApiRoutes (Route, getBook)
import Shared.Types (BookM, BookState)
import Simple.JSON (class WriteForeign, write)

port :: Int
port = 5000

runHandler ::
  forall req res.
  WriteForeign res =>
  BookState ->
  (Request req -> BookM (Response res)) ->
  HandlerM (Maybe req) ->
  Handler
runHandler state handler getRequest = do
  req <- getRequest
  case req of
    Just request -> do
      response <- liftAff $ runReaderT (handler { request: request }) state
      response # _.json # write # sendJson
    Nothing -> sendJson { error: "Invalid Request" }

addRoute ::
  forall req res.
  WriteForeign res =>
  (String -> Handler -> App) ->
  Route req ->
  (Request req -> BookM (Response res)) ->
  BookState ->
  App
addRoute verb route handler state = verb route.url $ runHandler state handler route.parseParams

server :: BookState -> App
server state = do
  addRoute get getBook getBookH state
  get "/" $ send "Hello World"

makeState :: Aff BookState
makeState = do
  conn <- dbConn
  pure { dbConn: Just conn, mocks: empty }

main :: Effect Unit
main =
  runAff_ (\_ -> pure unit) do
    state <- makeState
    liftEffect $ listenHttp (server state) port \_ -> log "Started"
