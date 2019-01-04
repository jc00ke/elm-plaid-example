port module Main exposing (Model, Msg(..), init, initialModel, itemLinked, main, openPlaidLink, subscriptions, update, userDecoder, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Text as Text
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D exposing (Decoder, map)
import Json.Encode as E



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { items : List Item
    , consentToNotify : Bool
    , consentForTransactions : Bool
    , name : String
    , stage : Stage
    }


initialModel : Model
initialModel =
    { items = []
    , consentToNotify = True
    , consentForTransactions = False
    , name = ""
    , stage = Start
    }


init : String -> ( Model, Cmd msg )
init name =
    ( { initialModel | name = name }, Cmd.none )



-- PORTS


port openPlaidLink : E.Value -> Cmd msg


port itemLinked : (E.Value -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    itemLinked (D.decodeValue itemDecoder >> GotItem)


type Msg
    = OpenPlaidLink
    | ToggleConsentToNotify
    | ToggleConsentForTransactions
    | GotItem (Result D.Error Item)



-- ROUTES


type Stage
    = Start
    | PickDepositoryAccount
    | InformPickTransactions
    | PickTransactionsAccounts
    | Finish



-- DECODERS


userDecoder : Decoder String
userDecoder =
    D.field "name" D.string


type Accounts
    = Accounts (List Account)


accountsDecoder : Decoder (List Account)
accountsDecoder =
    D.list accountDecoder


type alias Account =
    { name : String
    , id : String
    , aType : String
    , subtype : String
    , mask : String
    }


accountDecoder : Decoder Account
accountDecoder =
    D.map5
        Account
        (D.at [ "name" ] D.string)
        (D.at [ "id" ] D.string)
        (D.at [ "type" ] D.string)
        (D.at [ "subtype" ] D.string)
        (D.at [ "mask" ] D.string)


type alias Institution =
    { name : String
    , id : String
    }


institutionDecoder : Decoder Institution
institutionDecoder =
    D.map2
        Institution
        (D.at [ "name" ] D.string)
        (D.at [ "institution_id" ] D.string)


type alias Item =
    { public_token : String
    , institution : Institution
    , accounts : List Account
    }


itemDecoder : Decoder Item
itemDecoder =
    D.map3
        Item
        (D.field "public_token" D.string)
        (D.field "institution" institutionDecoder)
        (D.field "accounts" accountsDecoder)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotItem item ->
            case Result.toMaybe (Debug.log "item" item) of
                Nothing ->
                    ( Debug.log "Nothing" model, Cmd.none )

                Just a ->
                    ( Debug.log "item" { model | items = List.append model.items [ a ] }
                    , Cmd.none
                    )

        OpenPlaidLink ->
            ( model, openPlaidLink (E.list E.string [ "auth", "transactions" ]) )

        ToggleConsentToNotify ->
            ( { model | consentToNotify = not model.consentToNotify }
            , Cmd.none
            )

        ToggleConsentForTransactions ->
            ( { model | consentForTransactions = not model.consentForTransactions }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    Grid.container []
        [ Grid.row []
            [ Grid.col [] []
            , Grid.col [ Col.xs6 ] [ viewFor model ]
            , Grid.col [] []
            ]
        ]


viewFor : Model -> Html Msg
viewFor model =
    case model.stage of
        Start ->
            startView model

        PickDepositoryAccount ->
            pickDepositoryAccountView model

        InformPickTransactions ->
            informPickTransactionsView model

        PickTransactionsAccounts ->
            pickTransactionsAccountsView model

        Finish ->
            finishView model


startView : Model -> Html Msg
startView model =
    Card.config [ Card.align Text.alignXsCenter ]
        |> Card.block []
            [ enrollingText model
            , Block.text [] [ text "Log in to your primary bank (where the money will be deposited)" ]
            , Block.custom <|
                Button.button
                    [ Button.primary
                    , Button.attrs [ onClick <| OpenPlaidLink ]
                    ]
                    [ text "Get Started" ]
            ]
        |> Card.view


pickDepositoryAccountView : Model -> Html Msg
pickDepositoryAccountView model =
    Card.config [ Card.align Text.alignXsCenter ]
        |> Card.block []
            [ enrollingText model
            , Block.custom <|
                Grid.container []
                    (List.concat
                        [ [ Grid.row [] [ Grid.col [ Col.xs3 ] [ text "Deposit Into" ], Grid.col [] [] ] ]
                        , institutionRowForDepositorySelection model
                        , [ Grid.row [] [ Grid.col [] [ Button.button [ Button.secondary ] [ text "Link another bank" ] ], Grid.col [] [ Button.button [ Button.primary ] [ text "Next" ] ] ] ]
                        ]
                    )
            ]
        |> Card.view


informPickTransactionsView : Model -> Html Msg
informPickTransactionsView model =
    Card.config [ Card.align Text.alignXsCenter ]
        |> Card.block []
            [ enrollingText model
            , Block.text [] [ text "On the next screen, please select which accounts you want to share\ntransaction information with." ]
            , Block.custom <|
                Button.button
                    [ Button.primary, Button.attrs [] ]
                    [ text "Next" ]
            ]
        |> Card.view


pickTransactionsAccountsView : Model -> Html Msg
pickTransactionsAccountsView model =
    Card.config [ Card.align Text.alignXsCenter ]
        |> Card.block []
            [ enrollingText model
            , Block.custom <|
                Grid.container []
                    (List.concat
                        [ [ Grid.row [] [ Grid.col [ Col.lg, Col.textAlign Text.alignXsRight ] [ text "Transactions" ] ] ]
                        , institutionRowForTransactionSelection model
                        , [ Grid.row [] [ Grid.col [] [ Button.button [ Button.primary ] [ text "Finish" ] ] ] ]
                        ]
                    )
            ]
        |> Card.view


finishView : Model -> Html Msg
finishView model =
    div []
        [ Alert.simpleInfo []
            [ Alert.h4 [] [ text "Thank you for linking your accounts." ]
            , text "Please hand the device back to the associate"
            ]
        ]


enrollingText : Model -> Block.Item Msg
enrollingText model =
    Block.titleH3 [] [ "Enrolling " ++ model.name |> text ]


institutionRowForDepositorySelection : model -> List (Html Msg)
institutionRowForDepositorySelection model =
    [ Grid.row []
        [ Grid.col []
            [ h4 [ class "card-title" ] [ text "Platypus Bank" ] ]
        ]
    , Grid.row [ Row.leftSm ]
        [ Grid.col [ Col.xs3 ] [ Radio.radio [ Radio.name "depository" ] "" ]
        , Grid.col [ Col.textAlign Text.alignXsLeft ]
            [ h5 [] [ text "My Checking" ]
            , h6 [ class "mask" ] [ text "************0102" ]
            ]
        ]
    , Grid.row [ Row.leftSm ]
        [ Grid.col [ Col.xs3 ] [ Radio.radio [ Radio.name "depository" ] "" ]
        , Grid.col [ Col.textAlign Text.alignXsLeft ]
            [ h5 [] [ text "My Savings" ]
            , h6 [ class "mask" ] [ text "************3030" ]
            ]
        ]
    ]


institutionRowForTransactionSelection : model -> List (Html Msg)
institutionRowForTransactionSelection model =
    [ Grid.row []
        [ Grid.col []
            [ h4 [ class "card-title" ] [ text "Platypus Bank" ] ]
        ]
    , Grid.row [ Row.leftSm ]
        [ Grid.col [ Col.xs3 ] [ text "*" ]
        , Grid.col [ Col.textAlign Text.alignXsLeft ]
            [ h5 [] [ text "My Checking" ]
            , h6 [ class "mask" ] [ text "************0102" ]
            ]
        , Grid.col [ Col.xs3 ] [ Checkbox.checkbox [ Checkbox.id "transactions" ] "" ]
        ]
    , Grid.row [ Row.leftSm ]
        [ Grid.col [ Col.xs3 ] []
        , Grid.col [ Col.textAlign Text.alignXsLeft ]
            [ h5 [] [ text "My Savings" ]
            , h6 [ class "mask" ] [ text "************3030" ]
            ]
        , Grid.col [ Col.xs3 ] [ Checkbox.checkbox [ Checkbox.id "transactions" ] "" ]
        ]
    ]
