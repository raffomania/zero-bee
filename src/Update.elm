module Update exposing (update)

import Date
import Dict
import Model exposing (Model)
import Money
import Msg exposing (..)
import Set
import Storage
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

                        updatedModel =
                            { model
                                | transactions = updatedTransaction :: model.transactions
                                , newTransaction = { newTransaction | value = "" }
                            }
                    in
                    ( updatedModel
                    , Storage.storeModel updatedModel
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
            ( updatedModel, Storage.storeModel updatedModel )

        UpdateFromStorage value ->
            case Storage.decodeModel value of
                Ok newModel ->
                    ( { model | transactions = newModel.transactions, budgetEntries = newModel.budgetEntries }, Cmd.none )

                _ ->
                    Debug.todo "decoding failed"

        ChangeTransactionValue transaction value ->
            case Money.parse value of
                Just moneyValue ->
                    let
                        updateTransaction t =
                            if t == transaction then
                                { t | value = moneyValue }

                            else
                                t

                        updatedModel =
                            { model | transactions = List.map updateTransaction model.transactions }
                    in
                    ( updatedModel, Storage.storeModel updatedModel )

                _ ->
                    ( model, Cmd.none )

        NextMonth ->
            let
                current =
                    model.currentMonth

                updatedMonth =
                    case current.month of
                        Time.Dec ->
                            { month = Time.Jan, year = current.year + 1 }

                        _ ->
                            { current | month = Date.numberToMonth <| Date.monthToNumber current.month + 1 }
            in
            ( { model | currentMonth = updatedMonth }, Cmd.none )

        PreviousMonth ->
            let
                current =
                    model.currentMonth

                updatedMonth =
                    case current.month of
                        Time.Jan ->
                            { month = Time.Dec, year = current.year - 1 }

                        _ ->
                            { current | month = Date.numberToMonth <| Date.monthToNumber current.month - 1 }
            in
            ( { model | currentMonth = updatedMonth }, Cmd.none )

        RemoveTransaction transaction ->
            let
                updatedTransactions =
                    model.transactions
                        |> List.filter ((/=) transaction)

                updatedModel =
                    { model | transactions = updatedTransactions }
            in
            (updatedModel, Storage.storeModel updatedModel)
