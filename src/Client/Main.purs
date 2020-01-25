module Client.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Shared.ApiRoutes (getBook)
import Shared.Types (BookId(..))

main :: Effect Unit
main = do
  log $ getBook.makeUrl $ {bookId: BookId 1}
