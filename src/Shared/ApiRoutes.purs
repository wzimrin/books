module Shared.ApiRoutes where

import Prelude
import Data.Int (fromString)
import Data.Maybe (Maybe)
import Foreign (Foreign)
import Node.Express.Handler (HandlerM)
import Node.Express.Request (getRouteParam)
import Shared.Types (BookId(..), Book)

type Route requestT
  = { url :: String
    , makeUrl :: requestT -> String
    , parseParams :: HandlerM (Maybe requestT)
    }

type GetBookRequest
  = { bookId :: BookId }

type GetBookResponse
  = Maybe Book

getBook :: Route GetBookRequest
getBook =
  { url: "/book/:bookId"
  , makeUrl: _.bookId >>> show >>> ("/book/" <> _)
  , parseParams:
    do
      bookId <- getRouteParam "bookId"
      pure $ (bookId >>= makeRequest)
  }
  where
  makeRequest :: String -> Maybe GetBookRequest
  makeRequest idString = do
    id <- fromString idString
    pure $ { bookId: BookId id }
