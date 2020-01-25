module Shared.Types where

import Prelude
import Control.Monad.Reader (ReaderT)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Database.Postgres (Client)
import Database.Postgres.SqlValue (class IsSqlValue)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype BookId
  = BookId Int

derive newtype instance eqBookId :: Eq BookId

derive newtype instance showBookId :: Show BookId

derive newtype instance sqlValBookId :: IsSqlValue BookId

derive newtype instance readForeignBookId :: ReadForeign BookId

derive newtype instance writeForeignBookId :: WriteForeign BookId

type Book
  = { id :: BookId
    , title :: String
    , author :: String
    }

type BookState
  = { dbConn :: Maybe Client
    , mocks :: Map String Foreign
    }

type BookM
  = ReaderT BookState Aff
