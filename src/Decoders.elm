module Decoders exposing (accountDecoder, accountSubTypeDecoder, accountTypeDecoder, accountsDecoder, institutionDecoder, itemDecoder, userDecoder)

import Json.Decode as D exposing (Decoder, andThen, field, list, map2, map3, map5, string, succeed)
import Models exposing (..)


accountDecoder : Decoder Account
accountDecoder =
    map5
        Account
        (field "name" string)
        (field "id" string)
        (field "type" accountTypeDecoder)
        (field "subtype" accountSubTypeDecoder)
        (field "mask" string)


accountsDecoder : Decoder (List Account)
accountsDecoder =
    list accountDecoder


accountSubTypeDecoder : Decoder AccountSubType
accountSubTypeDecoder =
    string
        |> andThen
            (\str ->
                case str of
                    "cd" ->
                        succeed CD

                    "checking" ->
                        succeed Checking

                    "savings" ->
                        succeed Savings

                    "money market" ->
                        succeed MoneyMarket

                    "paypal" ->
                        succeed Paypal

                    "credit card" ->
                        succeed CreditCard

                    "rewards" ->
                        succeed Rewards

                    another ->
                        succeed OtherSubType
            )


accountTypeDecoder : Decoder AccountType
accountTypeDecoder =
    string
        |> andThen
            (\str ->
                case str of
                    "depository" ->
                        succeed Depository

                    "credit" ->
                        succeed Credit

                    "brokerage" ->
                        succeed Brokerage

                    "loan" ->
                        succeed Loan

                    other ->
                        succeed OtherType
            )


institutionDecoder : Decoder Institution
institutionDecoder =
    map2
        Institution
        (field "name" string)
        (field "institution_id" string)


itemDecoder : Decoder Item
itemDecoder =
    map3
        Item
        (field "public_token" string)
        (field "institution" institutionDecoder)
        (field "accounts" accountsDecoder)


userDecoder : Decoder String
userDecoder =
    field "name" string
