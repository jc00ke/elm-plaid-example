port module Main exposing (Model, Msg(..), init, initialModel, itemLinked, main, openPlaidLink, subscriptions, update, view)

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
import Decoders exposing (itemDecoder)
import Encoders exposing (encodeProducts)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (Error, decodeValue)
import Json.Encode as E
import Models exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type Msg
    = OpenPlaidLink (List Product)
    | GotItem (Result Error Item)
    | DepositInto String
    | ShowLinkAnotherBank
    | ShowFinish



-- MODEL


type alias Model =
    { items : List Item
    , name : String
    , stage : Stage
    , depositInto : Maybe String
    }


initialModel : Model
initialModel =
    { items = []
    , name = ""
    , stage = Start
    , depositInto = Nothing
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
    itemLinked (decodeValue itemDecoder >> GotItem)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotItem item ->
            case Result.toMaybe item of
                Nothing ->
                    ( Debug.log "Nothing" model, Cmd.none )

                Just a ->
                    let
                        stage =
                            nextStage model.stage
                    in
                    ( Debug.log "item" { model | items = List.append model.items [ a ], stage = stage }
                    , Cmd.none
                    )

        OpenPlaidLink products ->
            ( model, openPlaidLink (encodeProducts products) )

        DepositInto accountId ->
            ( { model | depositInto = Just accountId }, Cmd.none )

        ShowLinkAnotherBank ->
            let
                stage =
                    nextStage model.stage
            in
            ( Debug.log "item" { model | stage = stage }
            , Cmd.none
            )

        ShowFinish ->
            ( Debug.log "item" { model | stage = Finish }
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

        LinkAnotherBank ->
            linkAnotherBankView model

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
                    , Button.attrs [ onClick <| OpenPlaidLink [ Auth, Transactions ] ]
                    ]
                    [ text "Get Started" ]
            ]
        |> Card.view


pickDepositoryAccountView : Model -> Html Msg
pickDepositoryAccountView model =
    let
        disabled =
            case model.depositInto of
                Nothing ->
                    True

                Just _ ->
                    False
    in
    Card.config [ Card.align Text.alignXsCenter ]
        |> Card.block []
            [ enrollingText model
            , Block.custom <|
                Grid.container []
                    (List.concat
                        [ institutionRowsForDepositorySelection model
                        , [ Grid.row []
                                [ Grid.col [] [ Button.button [ Button.primary, Button.disabled disabled, Button.onClick ShowLinkAnotherBank ] [ text "Next" ] ]
                                ]
                          ]
                        ]
                    )
            ]
        |> Card.view


linkAnotherBankView : Model -> Html Msg
linkAnotherBankView model =
    Card.config [ Card.align Text.alignXsCenter ]
        |> Card.block []
            [ enrollingText model
            , Block.custom <|
                Grid.container []
                    [ Grid.row []
                        [ Grid.col [] [ Button.button [ Button.primary, Button.onClick <| OpenPlaidLink [ Transactions ] ] [ text "Link another bank" ] ]
                        , Grid.col [] [ Button.button [ Button.primary, Button.onClick ShowFinish ] [ text "Finish" ] ]
                        ]
                    ]
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


institutionRowsForDepositorySelection : Model -> List (Html Msg)
institutionRowsForDepositorySelection model =
    List.concatMap (\item -> institutionRowForDepositorySelection item model) model.items


institutionRowForDepositorySelection : Item -> Model -> List (Html Msg)
institutionRowForDepositorySelection item model =
    let
        rows =
            accountRowsForDepositorySelection item.accounts model
    in
    List.concat
        [ [ Grid.row [] [ Grid.col [] [ h4 [ class "card-title" ] [ text item.institution.name ] ] ] ]
        , rows
        ]


accountRowsForDepositorySelection : List Account -> Model -> List (Html Msg)
accountRowsForDepositorySelection accounts model =
    let
        depositoryAccounts =
            List.filter (\account -> account.type_ == Depository) accounts
    in
    List.concatMap (\account -> accountRowForDepositorySelection account model) depositoryAccounts


accountRowForDepositorySelection : Account -> Model -> List (Html Msg)
accountRowForDepositorySelection account model =
    let
        chosen =
            Just account.id == model.depositInto

        defaultRowClasses =
            "m-3 p-2 pointer"

        class_ =
            if chosen then
                "bg-light rounded shadow " ++ defaultRowClasses

            else
                defaultRowClasses
    in
    [ Grid.row [ Row.leftSm, Row.attrs [ class class_, onClick (DepositInto account.id) ] ]
        [ Grid.col [ Col.textAlign Text.alignXsLeft ]
            [ h5 [] [ text account.name ]
            , h6 [ class "mask" ] [ text <| "************" ++ account.mask ]
            ]
        ]
    ]


nextStage : Stage -> Stage
nextStage stage =
    case stage of
        Start ->
            PickDepositoryAccount

        PickDepositoryAccount ->
            LinkAnotherBank

        LinkAnotherBank ->
            Finish

        Finish ->
            Finish
