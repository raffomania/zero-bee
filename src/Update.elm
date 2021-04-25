module Update exposing (update)

import Date
import Dict
import Model exposing (Model)
import Month
import Msg exposing (..)
import Settings
import Storage
import StorageInterface
import Time
import Util exposing (dictUpsert)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ newTransaction } as model) =
    case msg of
        NewTime time ->
            let
                date =
                    Date.fromPosix Time.utc time
            in
            ( { model | date = date }, Cmd.none )

        Msg.SetCurrentMonth time ->
            let
                monthOfYear =
                    { month = Time.toMonth Time.utc time, year = Time.toYear Time.utc time }

                updatedMonth =
                    { model | currentMonth = monthOfYear }

                updatedNewTransactionDate =
                    update (AddTransactionNewDate newTransaction.dayOfMonth) updatedMonth
            in
            updatedNewTransactionDate

        AddTransaction ->
            let
                newTransactionDay =
                    newTransaction.dayOfMonth
                        |> String.toInt
                        |> Maybe.withDefault 1

                newTransactionDate =
                    newTransactionDay
                        |> Date.fromCalendarDate model.currentMonth.year model.currentMonth.month

                updatedTransaction =
                    { date = newTransactionDate
                    , value = newTransaction.value
                    , category = newTransaction.category
                    , note = newTransaction.note
                    , account = newTransaction.account
                    }

                updatedModel =
                    { model
                        | transactions = updatedTransaction :: model.transactions
                        , newTransaction = { newTransaction | value = 0, note = "" }
                    }
            in
            ( updatedModel
            , StorageInterface.storeModel updatedModel
            )

        AddTransactionNewValue value ->
            ( { model | newTransaction = { newTransaction | value = value } }, Cmd.none )

        AddTransactionNewCategory value ->
            ( { model | newTransaction = { newTransaction | category = value } }, Cmd.none )

        AddTransactionNewDate value ->
            ( { model | newTransaction = { newTransaction | dayOfMonth = value } }, Cmd.none )

        AddTransactionNewNote value ->
            ( { model | newTransaction = { newTransaction | note = value } }, Cmd.none )

        AddTransactionNewAccount value ->
            ( { model | newTransaction = { newTransaction | account = value } }, Cmd.none )

        AddTransactionFocusCategoryInput isFocused ->
            let
                newCategory =
                    if not isFocused then
                        Model.autocompletedCategories model
                            |> List.head
                            |> Maybe.withDefault model.newTransaction.category

                    else
                        model.newTransaction.category
            in
            ( { model | newTransaction = { newTransaction | categoryFocused = isFocused, category = newCategory } }, Cmd.none )

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
                    { model | budgetEntries = months, editingBudgetEntry = Nothing }
            in
            ( updatedModel, StorageInterface.storeModel updatedModel )

        UpdateFromStorage value ->
            case Storage.decodeModel value of
                Ok newModel ->
                    let
                        firstAccount =
                            newModel.transactions
                                |> List.head
                                |> Maybe.map (\t -> t.account)
                                |> Maybe.withDefault model.newTransaction.account

                        updatedNewTransaction =
                            { newTransaction | account = firstAccount }

                        updatedModel =
                            { model | transactions = newModel.transactions, budgetEntries = newModel.budgetEntries, newTransaction = updatedNewTransaction }

                        updatedWithSettings =
                            case newModel.settings of
                                Just settings ->
                                    { updatedModel | settings = settings }

                                _ ->
                                    updatedModel
                    in
                    ( updatedWithSettings, Cmd.none )

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
            ( updatedModel, StorageInterface.storeModel updatedModel )

        NextMonth ->
            ( { model | currentMonth = Month.increment model.currentMonth }, Cmd.none )

        PreviousMonth ->
            ( { model | currentMonth = Month.decrement model.currentMonth }, Cmd.none )

        RemoveTransaction transaction ->
            let
                -- TODO this removes duplicate transactions
                -- but should remove only one of them
                updatedTransactions =
                    model.transactions
                        |> List.filter ((/=) transaction)

                updatedModel =
                    { model | transactions = updatedTransactions }
            in
            ( updatedModel, StorageInterface.storeModel updatedModel )

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
            ( updatedModel, StorageInterface.storeModel updatedModel )

        ChangePage page ->
            ( { model | currentPage = page }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        ChangeEditedBudgetEntry month category value ->
            let
                default =
                    { category = category
                    , month = month
                    , value = value
                    }

                edit =
                    model.editingBudgetEntry
                        |> Maybe.andThen
                            (\e ->
                                if e.month /= month || e.category /= category then
                                    Nothing

                                else
                                    Just e
                            )
                        |> Maybe.withDefault default

                newEdit =
                    { edit | value = value }
            in
            ( { model | editingBudgetEntry = Just newEdit }, Cmd.none )

        Msg.ChangeSettings settingsMsg ->
            let
                ( updatedModel, cmd ) =
                    Settings.update settingsMsg model
            in
            ( updatedModel, Cmd.map Msg.ChangeSettings cmd )
