port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

-- MAIN


main =
  Browser.element {
    init = \() -> ( initialModel, Cmd.none ),
    subscriptions = subscriptions,
    update = update,
    view = view
  }

initialModel : Model
initialModel =
  { items = []
  , consentToNotify = True
  , consentForTransactions = False
  }



-- MODEL


type alias Model =
  { items : List String
  , consentToNotify : Bool
  , consentForTransactions : Bool
  }



init : () -> (Model, Cmd msg)
init _ =
  ( initialModel, Cmd.none )

port openPlaidLink : () -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



type Msg
  = OpenPlaidLink
  | ToggleConsentToNotify
  | ToggleConsentForTransactions



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OpenPlaidLink ->
      ( model, openPlaidLink () )

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
  div []
      [ div [] [ checkbox ToggleConsentToNotify "Receive SMS from us" ]
      , div [] [ checkbox ToggleConsentForTransactions "Make your transaction history available" ]
      , div [] [ button [ onClick OpenPlaidLink ] [ text "Link Account" ] ]
      ]

checkbox : msg -> String -> Html msg
checkbox msg name =
  label []
        [
          input [type_ "checkbox", onClick msg] []
        , text name
        ]

