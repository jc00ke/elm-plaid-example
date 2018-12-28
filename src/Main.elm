port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Text as Text
import Json.Encode as E
import Json.Decode as D exposing (Decoder, map)

-- MAIN


main =
  Browser.element {
    init = init,
    subscriptions = subscriptions,
    update = update,
    view = view
  }

initialModel : Model
initialModel =
  { items = []
  , consentToNotify = True
  , consentForTransactions = False
  , name = ""
  }



-- MODEL


type alias Model =
  { items : List String
  , consentToNotify : Bool
  , consentForTransactions : Bool
  , name : String
  }



init : () -> (Model, Cmd msg)
init _ =
  ( initialModel, Cmd.none )

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
          (model, Cmd.none)
        Just a ->
          (
            Debug.log "item" { model | name = a }
            , Cmd.none
          )

    OpenPlaidLink ->
      ( model, openPlaidLink (E.list E.string ["auth", "transactions"]) )

    ToggleConsentToNotify ->
      (
        { model | consentToNotify = not model.consentToNotify }
      , Cmd.none
      )


    ToggleConsentForTransactions ->
      (
        { model | consentForTransactions = not model.consentForTransactions }
      , Cmd.none
      )



-- VIEW


view : Model -> Html Msg
view model =
  --div []
      --[ div [] [ checkbox ToggleConsentToNotify "Receive SMS from us" ]
      --, div [] [ checkbox ToggleConsentForTransactions "Make your transaction history available" ]
      --, div [] [ button [ onClick OpenPlaidLink ] [ text "Link Account" ] ]
      --]
  Grid.container []
  [ Grid.row []
    [ Grid.col [] []
    , Grid.col [ Col.xs6 ] [
      div []
          [ Card.config [ Card.align Text.alignXsCenter ]
              |> Card.block []
                            [ Block.titleH3 [] [ text "Enrolling Jesse Cooke" ]
                            , Block.text []
                              [ text "Log in to your primary bank (where the money will be deposited)" ]
                            , Block.custom <|
                                Button.button
                                [ Button.primary
                                , Button.attrs [ onClick <| OpenPlaidLink ] ] [ text "Get Started" ]
                            ]
              |> Card.view
          ]
      ]
    , Grid.col [] []
    ]
  ]

checkbox : msg -> String -> Html msg
checkbox msg name =
  label []
        [
          input [type_ "checkbox", onClick msg] []
        , text name
        ]

