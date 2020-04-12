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

        AddTransaction ->
            let
                maybeDate =
                    Date.fromIsoString model.newTransaction.date
                maybeValue =
                    String.toInt model.newTransaction.value
            in
            case (maybeDate, maybeValue) of
                (Ok date, Just value) ->
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
