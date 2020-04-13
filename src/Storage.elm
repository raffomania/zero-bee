port module Storage exposing (..)

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


port storeModel : E.Value -> Cmd msg


port modelUpdated : (E.Value -> msg) -> Sub msg


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
    D.int
        |> D.map Time.millisToPosix
        |> D.map (Date.fromPosix Time.utc)


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
