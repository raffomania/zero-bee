module Update exposing (update)

import Date
import Dict
import Model exposing (Model)
import Money
import Month
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
            in
            case maybeDate of
                Ok date ->
                    let
                        transaction =
                            model.newTransaction

                        updatedTransaction =
                            { date = date
                            , value = model.newTransaction.value
                            , category = transaction.category
                            }

                        updatedModel =
                            { model
                                | transactions = updatedTransaction :: model.transactions
                                , newTransaction = { newTransaction | value = 0 }
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
                monthIndex =
                    Model.getMonthIndex month

                defaultEntry =
                    { value = value, category = category }

                updateMonth =
                    \monthDict ->
                        dictUpsert
                            category
                            (\e -> { e | value = value })
                            defaultEntry
                            monthDict

                months =
                    dictUpsert
                        monthIndex
                        updateMonth
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
                    ( model, Cmd.none )

        ChangeTransactionValue transaction value ->
            let
                updateTransaction t =
                    if t == transaction then
                        { t | value = value }

                    else
                        t

                updatedModel =
                    { model | transactions = List.map updateTransaction model.transactions }
            in
            ( updatedModel, Storage.storeModel updatedModel )

        NextMonth ->
            ( { model | currentMonth = Month.increment model.currentMonth }, Cmd.none )

        PreviousMonth ->
            ( { model | currentMonth = Month.decrement model.currentMonth }, Cmd.none )

        RemoveTransaction transaction ->
            let
                updatedTransactions =
                    model.transactions
                        |> List.filter ((/=) transaction)

                updatedModel =
                    { model | transactions = updatedTransactions }
            in
            ( updatedModel, Storage.storeModel updatedModel )

        RemoveBudgetEntry month category ->
            let
                updateMonth =
                    Maybe.map (Dict.remove category)

                updatedEntries =
                    Dict.update (Model.getMonthIndex month)
                        updateMonth
                        model.budgetEntries

                updatedModel =
                    { model | budgetEntries = updatedEntries }
            in
            ( updatedModel, Storage.storeModel updatedModel )

        ChangePage page ->
            ( { model | currentPage = page }, Cmd.none )
