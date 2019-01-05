module Encoders exposing (encodeProducts)

import Json.Encode exposing (Value, list, string)
import Models exposing (Product(..))


encodeProduct : Product -> Value
encodeProduct product =
    case product of
        Auth ->
            string "auth"

        Transactions ->
            string "transactions"


encodeProducts : List Product -> Value
encodeProducts products =
    list encodeProduct products
