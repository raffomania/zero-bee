module Update exposing (update)

import Date
import Model exposing (Model)
import Msg exposing (..)
import Set
import Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ newTransaction } as model) =
    case msg of
        NewTime time ->
            let
                monthOfYear =
                    { month = Time.toMonth Time.utc time, year = Time.toYear Time.utc time }
            in
            ( { model | currentMonth = monthOfYear }, Cmd.none )

        NewCategoryText text ->
            ( { model | newCategory = text }, Cmd.none )

        AddCategory ->
            ( { model
                | categories = Set.insert model.newCategory model.categories
                , newCategory = ""
              }
            , Cmd.none
            )

        AddTransaction ->
            case model.newTransaction.date of
                Nothing ->
                    ( model, Cmd.none )

                Just date ->
                    let
                        transaction =
                            model.newTransaction

                        updatedTransaction =
                            { date = date
                            , value = transaction.value
                            , category = transaction.category
                            }
                    in
                    ( { model
                        | transactions = updatedTransaction :: model.transactions
                        , newTransaction = { newTransaction | value = 0 }
                      }
                    , Cmd.none
                    )

        AddTransactionNewValue value ->
            let
                newValue =
                    Maybe.withDefault newTransaction.value <| String.toInt value
            in
            ( { model | newTransaction = { newTransaction | value = newValue } }, Cmd.none )

        AddTransactionNewCategory value ->
            ( { model | newTransaction = { newTransaction | category = value } }, Cmd.none )

        AddTransactionNewDate value ->
            let
                newModel =
                    case Date.fromIsoString value of
                        Ok date ->
                            { model | newTransaction = { newTransaction | date = Just date } }

                        Err _ ->
                            model
            in
            ( newModel, Cmd.none )
