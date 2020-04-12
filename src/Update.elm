module Update exposing (update)

import Date
import Dict
import Model exposing (Model)
import Msg exposing (..)
import Set
import Time
import Util exposing (dictUpsert)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ newTransaction } as model) =
    case msg of
        NewTime time ->
            let
                monthOfYear =
                    { month = Time.toMonth Time.utc time, year = Time.toYear Time.utc time }
            in
            ( { model | currentMonth = monthOfYear }, Cmd.none )

        AddTransaction ->
            let
                maybeDate =
                    Date.fromIsoString model.newTransaction.date

                maybeValue =
                    String.toInt model.newTransaction.value
            in
            case ( maybeDate, maybeValue ) of
                ( Ok date, Just value ) ->
                    let
                        transaction =
                            model.newTransaction

                        updatedTransaction =
                            { date = date
                            , value = value
                            , category = transaction.category
                            }
                    in
                    ( { model
                        | transactions = updatedTransaction :: model.transactions
                        , newTransaction = { newTransaction | value = "" }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AddTransactionNewValue value ->
            ( { model | newTransaction = { newTransaction | value = value } }, Cmd.none )

        AddTransactionNewCategory value ->
            ( { model | newTransaction = { newTransaction | category = value } }, Cmd.none )

        AddTransactionNewDate value ->
            ( { model | newTransaction = { newTransaction | date = value } }, Cmd.none )

        ChangeBudgetEntry month category value ->
            let
                newValue =
                    value |> String.toInt |> Maybe.withDefault 0

                monthIndex =
                    Model.getMonthIndex month

                defaultEntry =
                    { month = month, value = newValue, category = category }

                updateMonth =
                    \monthDict ->
                        dictUpsert
                            category
                            (Maybe.map (\e -> { e | value = newValue }))
                            defaultEntry
                            monthDict

                months =
                    dictUpsert
                        monthIndex
                        (Maybe.map updateMonth)
                        (Dict.singleton category defaultEntry)
                        model.budgetEntries

                updatedModel =
                    { model | budgetEntries = months }
            in
            ( updatedModel, Cmd.none )
