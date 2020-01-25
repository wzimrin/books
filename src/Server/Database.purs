module Server.Database where

import Prelude
import Control.Alternative ((<|>))
import Control.Monad.Reader (ask)
import Data.Either (Either(..), either, hush)
import Data.Foldable (intercalate)
import Data.Map (lookup)
import Data.Maybe (Maybe, fromMaybe)
import Database.Postgres
  ( Client
  , ClientConfig
  , Query(..)
  , connect
  , connectionInfoFromConfig
  , defaultPoolConfig
  , mkPool
  , queryOne
  )
import Database.Postgres.SqlValue (toSql)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, throw)
import Foreign (Foreign, MultipleErrors, renderForeignError)
import Shared.Types (Book, BookId, BookM)
import Simple.JSON (class ReadForeign, read)

dbConfig :: ClientConfig
dbConfig =
  { database: "book-db"
  , host: "localhost"
  , password: "book"
  , port: 5432
  , ssl: false
  , user: "bookapp"
  }

dbConn :: Aff Client
dbConn = do
  pool <- liftEffect $ mkPool connectionInfo
  connect pool
  where
  connectionInfo = connectionInfoFromConfig dbConfig defaultPoolConfig

mockable :: forall a. ReadForeign a => String -> (Client -> BookM a) -> BookM a
mockable id inner = do
  state <- ask
  let
    mockVal = lookup id state.mocks >>= read >>> hush # map pure

    realVal = inner <$> state.dbConn

    err = liftEffect $ throw $ "No or invalid mock found for " <> id
  fromMaybe err $ mockVal <|> realVal

translateError :: forall a. Either MultipleErrors a -> Either Error a
translateError = either (Left <<< renderErrors) Right
  where
  renderErrors = map renderForeignError >>> intercalate "\n" >>> error

dbRead :: forall t. ReadForeign t => Foreign -> Either Error t
dbRead = read >>> translateError

getBook :: BookId -> BookM (Maybe Book)
getBook id =
  mockable "database.getBook" \conn -> do
    liftAff $ queryOne dbRead (Query "select * from books where id = $1") [ toSql id ] conn
