port module Storage exposing (decodeModel, modelUpdated, storeModel)

import Date exposing (Date)
import Debug
import Dict exposing (Dict)
import Json.Decode as D exposing (field)
import Json.Encode as E
import Model exposing (..)
import Time


type alias StoredModel =
    { transactions : List Transaction
    , budgetEntries : Dict MonthIndex (Dict CategoryId BudgetEntry)
    }


port sendModel : E.Value -> Cmd msg


port modelUpdated : (E.Value -> msg) -> Sub msg


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
        ]


encodeTransaction transaction =
    E.object
        [ ( "value", E.int transaction.value )
        , ( "date", encodeDate transaction.date )
        , ( "category", E.string transaction.category )
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
        [ ( "month", E.object [ ( "month", E.int <| Date.monthToNumber entry.month.month ), ( "year", E.int entry.month.year ) ] )
        , ( "value", E.int entry.value)
        , ( "category", E.string entry.category)
        ]


decodeModel : D.Value -> Result D.Error StoredModel
decodeModel val =
    D.decodeValue modelDecoder val


modelDecoder : D.Decoder StoredModel
modelDecoder =
    D.map2 StoredModel
        (field "transactions" <| D.list transactionDecoder)
        (field "budgetEntries" <| D.dict budgetMonthDecoder)


transactionDecoder : D.Decoder Transaction
transactionDecoder =
    D.map3 Transaction
        (field "value" D.int)
        (field "date" dateDecoder)
        (field "category" D.string)


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
    D.map3 BudgetEntry
        (field "month" (D.map2 MonthOfYear (field "month" monthDecoder) (field "year" D.int)))
        (field "value" D.int)
        (field "category" D.string)


monthDecoder =
    D.map Date.numberToMonth D.int
