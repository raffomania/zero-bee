module Storage exposing (decodeModel, encodeModel)

import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode as D exposing (field)
import Json.Encode as E
import Model exposing (Model)


type alias StoredModel =
    { transactions : List Model.Transaction
    , budgetEntries : Dict Model.MonthIndex (Dict Model.CategoryId Model.BudgetEntry)
    , settings : Maybe Model.Settings
    }


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


encodeTransaction : Model.Transaction -> E.Value
encodeTransaction transaction =
    E.object
        [ ( "value", E.int transaction.value )
        , ( "date", encodeDate transaction.date )
        , ( "category", E.string transaction.category )
        , ( "note", E.string transaction.note )
        , ( "account", E.string transaction.account)
        ]


encodeDate : Date.Date -> E.Value
encodeDate date =
    date
        |> Date.toIsoString
        |> E.string


encodeBudgetDict : Dict Model.MonthIndex Model.BudgetEntry -> E.Value
encodeBudgetDict dict =
    E.dict identity encodeBudgetEntry dict


encodeBudgetEntry : Model.BudgetEntry -> E.Value
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


settingsDecoder : D.Decoder Model.Settings
settingsDecoder =
    D.map2 Model.Settings
        (field "currencySymbol" D.string)
        (field "syncAddress" D.string)


transactionDecoder : D.Decoder Model.Transaction
transactionDecoder =
    D.map5 Model.Transaction
        (field "value" D.int)
        (field "date" dateDecoder)
        (field "category" D.string)
        (D.oneOf [ field "note" D.string, D.succeed "" ])
        (D.oneOf [field "account" D.string, D.succeed ""])


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


budgetMonthDecoder : D.Decoder (Dict Model.CategoryId Model.BudgetEntry)
budgetMonthDecoder =
    D.dict budgetEntryDecoder


budgetEntryDecoder : D.Decoder Model.BudgetEntry
budgetEntryDecoder =
    D.map2 Model.BudgetEntry
        (field "value" D.int)
        (field "category" D.string)