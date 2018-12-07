import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (
  Decoder,
  at,
  field,
  int,
  lazy,
  list,
  map,
  map2,
  map3,
  map4,
  map5,
  string)



-- MAIN


main =
  Browser.element {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }



-- MODEL

type alias Account =
  { id : String
  , name : String
  , mask : String
  , typeName : String
  , subtype : String
  }

type alias Institution =
  { name : String
  , id : String
  }

type alias Metadata =
  { linkSessionId : String
  , publicToken : String
  , institution : Institution
  , primaryAccount : Account
  , accounts : Accounts
  }

type Accounts = Accounts (List Account)

type alias Model =
  { items : List Metadata
  , consentToNotify : Bool
  , consentForTransactions : Bool
  }



-- DECODER

account : Decoder Account
account =
  map5 Account
    (field "id" string)
    (field "name" string)
    (field "mask" string)
    (field "typeName" string)
    (field "subtype" string)

institution : Decoder Institution
institution =
  map2 Institution
  (field "id" string)
  (field "name" string)

metadata : Decoder Metadata
metadata =
  map5 Metadata
  (field "link_session_id" string)
  (field "public_token" string)
  (field "institution" institution)
  (field "account" account)
  (field "accounts" (map Accounts (list (lazy (\_ -> account)))))


init : () -> (Model, Cmd Msg)
init _ =
  ( Model [] True True
  , Cmd.none )



-- UPDATE


type Msg
  = ToggleConsentToNotify
  | ToggleConsentForTransactions

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  div [] []
  --fieldset []
    --[ checkbox ToggleConsentToNotify "Consent to receive SMS messages"
    --, checkbox ToggleConsentForTransactions "Consent to give us your transaction history"
    --]

checkbox : msg -> String -> Html msg
checkbox msg name =
  label []
    [ input [ type_ "checkbox", onClick msg ] []
    , text name
    ]
