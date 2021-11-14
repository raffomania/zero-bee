module Budget exposing (..)

import Date
import Expect
import Main
import Model exposing (Transaction, monthToDate)
import Month
import Test exposing (..)
import Test.Html.Selector exposing (..)
import View.Budget


defaultModel : Model.Model
defaultModel =
    Main.init {}
        |> Tuple.first


previousMonth : Date.Date
previousMonth =
    defaultModel.currentMonth |> Month.decrement |> monthToDate


pastActivity : Test
pastActivity =
    let
        transactions : List Transaction
        transactions =
            [ { value = 10, date = previousMonth, category = "foo", note = "", account = "" } ]

        ( activeRows, _ ) =
            View.Budget.calculateBudgetRows { defaultModel | transactions = transactions }

        row =
            List.head activeRows
    in
    describe "given a transaction in the past with no budget entries"
        [ test "Displays the corresponding budget entry with zero activity but some money available"
            (\_ ->
                row
                    |> Expect.equal
                        (Just
                            { category = "foo"
                            , budgeted = 0
                            , activity = 0
                            , available = 10
                            }
                        )
            )
        ]


inactiveEntry : Test
inactiveEntry =
    let
        twoMonthsAgo =
            Month.decrement (Month.decrement defaultModel.currentMonth)

        transactions : List Transaction
        transactions =
            [ { value = 10, date = monthToDate twoMonthsAgo, category = "foo", note = "", account = "" }, { value = -10, date = monthToDate twoMonthsAgo, category = "foo", note = "", account = "" } ]

        ( _, inactiveRows ) =
            View.Budget.calculateBudgetRows { defaultModel | transactions = transactions }

        row =
            List.head inactiveRows
    in
    describe "given transactions two months in the past that sum to zero activity"
        [ test "The budget entry is displayed as an inactive entry"
            (\_ ->
                row
                    |> Expect.equal
                        (Just
                            { category = "foo"
                            , budgeted = 0
                            , activity = 0
                            , available = 0
                            }
                        )
            )
        ]


activeZeroEntry : Test
activeZeroEntry =
    let
        transactions : List Transaction
        transactions =
            [ { value = 10, date = previousMonth, category = "foo", note = "", account = "" }, { value = -10, date = previousMonth, category = "foo", note = "", account = "" } ]

        ( activeRows, _ ) =
            View.Budget.calculateBudgetRows { defaultModel | transactions = transactions }

        row =
            List.head activeRows
    in
    describe "given transactions a month in the past that sum to zero activity"
        [ test "The budget entry is displayed as an active entry"
            (\_ ->
                row
                    |> Expect.equal
                        (Just
                            { category = "foo"
                            , budgeted = 0
                            , activity = 0
                            , available = 0
                            }
                        )
            )
        ]
