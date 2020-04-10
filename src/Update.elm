module Update exposing (update)

import Model exposing (Model)
import Msg exposing (..)
import Set
import Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
            ( { model
                | transactions = model.newTransaction :: model.transactions
              }
            , Cmd.none
            )

        AddTransactionNewValue value ->
            let
                transaction =
                    model.newTransaction

                updatedTransaction =
                    { transaction | value = Maybe.withDefault transaction.value <| String.toInt value }
            in
            ( { model | newTransaction = updatedTransaction }, Cmd.none )
