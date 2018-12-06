import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
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
  , institution : Institution
  , accounts : Accounts
  }

type Accounts = Accounts (List Account)

type alias Item =
  { publicToken : String
  , metadata : Metadata
  }

type Items = Items (List Item)

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
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
  map3 Metadata
  (field "link_session_id" string)
  (field "institution" institution)
  (field "accounts" (map Accounts (list (lazy (\_ -> account)))))

item : Decoder Item
item =
  map2 Item
  (field "public_token" string)
  (field "metadata" metadata)


init : () -> (List Item, Cmd Msg)
init _ =
  ( []
  , Cmd.none
  )



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String

update : Msg -> List Item -> (List Item, Cmd Msg)
update msg model =
  (model, Cmd.none)




-- SUBSCRIPTIONS


subscriptions : List Item -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : List Item -> Html Msg
view model =
  div [] []
