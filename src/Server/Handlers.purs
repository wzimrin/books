module Server.Handlers where

import Prelude
import Server.Database (getBook)
import Shared.ApiRoutes (GetBookRequest, GetBookResponse)
import Shared.Types (BookM)

type Request t
  = { request :: t }

type Response t
  = { json :: t }

success :: forall t. t -> BookM (Response t)
success json = pure { json: json }

getBookH :: Request GetBookRequest -> BookM (Response GetBookResponse)
getBookH req = do
  getBook req.request.bookId >>= success
