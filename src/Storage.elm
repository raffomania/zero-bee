port module Storage exposing (connect, decodeModel, modelUpdated, storeModel)

import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode as D exposing (field)
import Json.Encode as E
import Model exposing (..)


type alias StoredModel =
    { transactions : List Transaction
    , budgetEntries : Dict MonthIndex (Dict CategoryId BudgetEntry)
    , settings : Maybe SettingsData
    }


port sendModel : E.Value -> Cmd msg


port modelUpdated : (E.Value -> msg) -> Sub msg


port connect : String -> Cmd msg


storeModel : Model -> Cmd msg
storeModel model =
    model
        |> encodeModel
        |> sendModel


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "transactions", E.list encodeTransaction model.transactions )
        , ( "budgetEntries", E.dict identity encodeBudgetDict model.budgetEntries )
        , ( "settings"
          , E.object
                [ ( "syncAddress", E.string model.settings.syncAddress )
                , ( "currencySymbol", E.string model.settings.currencySymbol )
                ]
          )
        ]


encodeTransaction : Transaction -> E.Value
encodeTransaction transaction =
    E.object
        [ ( "value", E.int transaction.value )
        , ( "date", encodeDate transaction.date )
        , ( "category", E.string transaction.category )
        , ( "note", E.string transaction.note )
        ]


encodeDate : Date.Date -> E.Value
encodeDate date =
    date
        |> Date.toIsoString
        |> E.string


encodeBudgetDict dict =
    E.dict identity encodeBudgetEntry dict


encodeBudgetEntry : BudgetEntry -> E.Value
encodeBudgetEntry entry =
    E.object
        [ ( "value", E.int entry.value )
        , ( "category", E.string entry.category )
        ]


decodeModel : D.Value -> Result D.Error StoredModel
decodeModel val =
    D.decodeValue modelDecoder val


modelDecoder : D.Decoder StoredModel
modelDecoder =
    D.map3 StoredModel
        (field "transactions" <| D.list transactionDecoder)
        (field "budgetEntries" <| D.dict budgetMonthDecoder)
        (D.maybe (field "settings" settingsDecoder))


settingsDecoder : D.Decoder SettingsData
settingsDecoder =
    D.map2 SettingsData
        (field "currencySymbol" D.string)
        (field "syncAddress" D.string)


transactionDecoder : D.Decoder Transaction
transactionDecoder =
    D.map4 Transaction
        (field "value" D.int)
        (field "date" dateDecoder)
        (field "category" D.string)
        (D.oneOf [ field "note" D.string, D.succeed "" ])


dateDecoder : D.Decoder Date
dateDecoder =
    D.string
        |> D.map Date.fromIsoString
        |> D.andThen resultDecoder


resultDecoder : Result String val -> D.Decoder val
resultDecoder res =
    case res of
        Ok val ->
            D.succeed val

        Err desc ->
            D.fail desc


budgetMonthDecoder : D.Decoder (Dict CategoryId BudgetEntry)
budgetMonthDecoder =
    D.dict budgetEntryDecoder


budgetEntryDecoder : D.Decoder BudgetEntry
budgetEntryDecoder =
    D.map2 BudgetEntry
        (field "value" D.int)
        (field "category" D.string)


monthDecoder =
    D.map Date.numberToMonth D.int
