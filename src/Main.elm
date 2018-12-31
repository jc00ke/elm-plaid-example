port module Main exposing (Model, Msg(..), checkbox, init, initialModel, itemLinked, main, openPlaidLink, subscriptions, update, userDecoder, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
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
    { items : List String
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
    itemLinked (D.decodeValue userDecoder >> GotItem)


type Msg
    = OpenPlaidLink
    | ToggleConsentToNotify
    | ToggleConsentForTransactions
    | GotItem (Result D.Error String)



-- ROUTES


type Stage
    = Start
    | PickDepositoryAccount
    | Inform_PickTransactions
    | PickTransactionsAccounts
    | Finish



-- DECODERS


userDecoder : Decoder String
userDecoder =
    D.field "name" D.string



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotItem item ->
            case Result.toMaybe item of
                Nothing ->
                    ( model, Cmd.none )

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
            , Grid.col [ Col.xs6 ]
                [ div [] [ viewFor model ] ]
            , Grid.col [] []
            ]
        ]


viewFor : Model -> Html Msg
viewFor model =
    case model.stage of
        Start ->
            startView model

        _ ->
            elseView model


startView : Model -> Html Msg
startView model =
    Card.config [ Card.align Text.alignXsCenter ]
        |> Card.block []
            [ Block.titleH3 [] [ "Enrolling " ++ model.name |> text ]
            , Block.text []
                [ text "Log in to your primary bank (where the money will be deposited)" ]
            , Block.custom <|
                Button.button
                    [ Button.primary
                    , Button.attrs [ onClick <| OpenPlaidLink ]
                    ]
                    [ text "Get Started" ]
            ]
        |> Card.view


elseView : Model -> Html Msg
elseView model =
    div []
        [ Alert.simpleDanger [] [ text "IMPLEMENT" ] ]


checkbox : msg -> String -> Html msg
checkbox msg name =
    label []
        [ input [ type_ "checkbox", onClick msg ] []
        , text name
        ]
