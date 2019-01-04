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
    , name : String
    , stage : Stage
    , depositInto : String
    , nextEnabled : Bool
    }


initialModel : Model
initialModel =
    { items = []
    , name = ""
    , stage = Start
    , depositInto = ""
    , nextEnabled = False
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
    = OpenPlaidLink (List Product)
    | GotItem (Result D.Error Item)
    | DepositInto String
    | ShowInformPickTransactions
    | ShowPickTransactionsAccounts



-- ROUTES


type Stage
    = Start
    | PickDepositoryAccount
    | InformPickTransactions
    | PickTransactionsAccounts
    | Finish



-- TYPES


type Product
    = Auth
    | Transactions



-- DECODERS & TYPES


userDecoder : Decoder String
userDecoder =
    D.field "name" D.string


type AccountType
    = Depository
    | Credit
    | Brokerage
    | Loan
    | OtherType


accountTypeDecoder : Decoder AccountType
accountTypeDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "depository" ->
                        D.succeed Depository

                    "credit" ->
                        D.succeed Credit

                    "brokerage" ->
                        D.succeed Brokerage

                    "loan" ->
                        D.succeed Loan

                    other ->
                        D.succeed OtherType
            )


type AccountSubType
    = CD
    | Checking
    | Savings
    | MoneyMarket
    | Paypal
    | Prepaid
    | CreditCard
    | Rewards
    | OtherSubType


accountSubTypeDecoder : Decoder AccountSubType
accountSubTypeDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "cd" ->
                        D.succeed CD

                    "checking" ->
                        D.succeed Checking

                    "savings" ->
                        D.succeed Savings

                    "money market" ->
                        D.succeed MoneyMarket

                    "paypal" ->
                        D.succeed Paypal

                    "credit card" ->
                        D.succeed CreditCard

                    "rewards" ->
                        D.succeed Rewards

                    another ->
                        D.succeed OtherSubType
            )


type Accounts
    = Accounts (List Account)


accountsDecoder : Decoder (List Account)
accountsDecoder =
    D.list accountDecoder


type alias Account =
    { name : String
    , id : String
    , type_ : AccountType
    , subType : AccountSubType
    , mask : String
    }


accountDecoder : Decoder Account
accountDecoder =
    D.map5
        Account
        (D.field "name" D.string)
        (D.field "id" D.string)
        (D.field "type" accountTypeDecoder)
        (D.field "subtype" accountSubTypeDecoder)
        (D.field "mask" D.string)


type alias Institution =
    { name : String
    , id : String
    }


institutionDecoder : Decoder Institution
institutionDecoder =
    D.map2
        Institution
        (D.field "name" D.string)
        (D.field "institution_id" D.string)


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


encodeProduct : Product -> E.Value
encodeProduct product =
    case product of
        Auth ->
            E.string "auth"

        Transactions ->
            E.string "transactions"


encodeProducts : List Product -> E.Value
encodeProducts products =
    E.list encodeProduct products



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
            ( { model | nextEnabled = False }, openPlaidLink (encodeProducts [ Auth, Transactions ]) )

        DepositInto accountId ->
            ( { model | depositInto = accountId, nextEnabled = True }, Cmd.none )

        ShowInformPickTransactions ->
            let
                stage =
                    nextStage model.stage
            in
            ( Debug.log "item" { model | stage = stage }
            , Cmd.none
            )

        ShowPickTransactionsAccounts ->
            let
                stage =
                    nextStage model.stage
            in
            ( Debug.log "item" { model | stage = stage }
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
                    , Button.attrs [ onClick <| OpenPlaidLink [ Auth, Transactions ] ]
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
                        , institutionRowsForDepositorySelection model.items
                        , [ Grid.row []
                                [ Grid.col []
                                    [ Button.button [ Button.secondary, Button.onClick <| OpenPlaidLink [ Transactions ] ] [ text "Link another bank" ] ]
                                , Grid.col [] [ Button.button [ Button.primary, Button.disabled (not model.nextEnabled), Button.onClick ShowInformPickTransactions ] [ text "Next" ] ]
                                ]
                          ]
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
                    [ Button.primary, Button.attrs [ onClick ShowPickTransactionsAccounts ] ]
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


institutionRowsForDepositorySelection : List Item -> List (Html Msg)
institutionRowsForDepositorySelection items =
    List.concatMap institutionRowForDepositorySelection items


institutionRowForDepositorySelection : Item -> List (Html Msg)
institutionRowForDepositorySelection item =
    let
        rows =
            accountRowsForDepositorySelection item.accounts
    in
    List.concat
        [ [ Grid.row [] [ Grid.col [] [ h4 [ class "card-title" ] [ text item.institution.name ] ] ] ]
        , rows
        ]


accountRowsForDepositorySelection : List Account -> List (Html Msg)
accountRowsForDepositorySelection accounts =
    let
        depositoryAccounts =
            List.filter (\account -> account.type_ == Depository) accounts
    in
    List.concatMap accountRowForDepositorySelection depositoryAccounts


accountRowForDepositorySelection : Account -> List (Html Msg)
accountRowForDepositorySelection account =
    [ Grid.row [ Row.leftSm ]
        [ Grid.col [ Col.xs3 ] [ Radio.radio [ Radio.name "depository", Radio.onClick (DepositInto account.id) ] "" ]
        , Grid.col [ Col.textAlign Text.alignXsLeft ]
            [ h5 [] [ text account.name ]
            , h6 [ class "mask" ] [ text <| "************" ++ account.mask ]
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


nextStage : Stage -> Stage
nextStage stage =
    case stage of
        Start ->
            PickDepositoryAccount

        PickDepositoryAccount ->
            InformPickTransactions

        InformPickTransactions ->
            PickTransactionsAccounts

        PickTransactionsAccounts ->
            Finish

        Finish ->
            Finish
