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
                        Model.autocompletedCategories model model.newTransaction.category
                            |> List.head
                            |> Maybe.withDefault model.newTransaction.category

                    else
                        model.newTransaction.category

                newFocus =
                    if isFocused then
                        Just Model.Category

                    else
                        Nothing
            in
            ( { model | newTransaction = { newTransaction | focus = newFocus, category = newCategory } }, Cmd.none )

        AddTransactionFocusAccountInput isFocused ->
            let
                newAccount =
                    if not isFocused then
                        Model.autocompletedAccounts model
                            |> List.head
                            |> Maybe.withDefault model.newTransaction.account

                    else
                        model.newTransaction.account

                newFocus =
                    if isFocused then
                        Just Model.Account

                    else
                        Nothing
            in
            ( { model | newTransaction = { newTransaction | focus = newFocus, account = newAccount } }, Cmd.none )

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

        ChangeEditingTransactionCategory transaction category ->
            let
                updatedEdit =
                    Just
                        { transaction = transaction
                        , input = category
                        , focus = Model.Category
                        }

                updatedModel =
                    { model | editingTransaction = updatedEdit }
            in
            ( updatedModel, StorageInterface.storeModel updatedModel )

        ChangeEditingTransactionFocus transaction isFocused ->
            let
                input =
                    model.editingTransaction |> Maybe.map .input |> Maybe.withDefault transaction.category

                newCategory =
                    if not isFocused then
                        Model.autocompletedCategories model input
                            |> List.head
                            |> Maybe.withDefault input

                    else
                        transaction.category

                updatedEdit =
                    if isFocused then
                        Just
                            { transaction = transaction
                            , focus = Model.Category
                            , input = input
                            }

                    else
                        Nothing

                updateTransaction t =
                    if t == transaction then
                        { t | category = newCategory }

                    else
                        t

                updatedModel =
                    { model | transactions = List.map updateTransaction model.transactions, editingTransaction = updatedEdit }
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
