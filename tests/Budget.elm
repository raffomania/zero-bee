module Budget exposing (..)

import Expect
import Main
import Model exposing (Transaction, monthToDate)
import Month
import Test exposing (..)
import Test.Html.Selector exposing (..)
import View.Budget


pastActivity : Test
pastActivity =
    let
        model =
            Main.init {}
                |> Tuple.first

        previousMonth =
            model.currentMonth |> Month.decrement |> monthToDate

        transactions : List Transaction
        transactions =
            [ { value = 10, date = previousMonth, category = "foo", note = "", account = "" } ]

        ( activeRows, _ ) =
            View.Budget.calculateBudgetRows { model | transactions = transactions }

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
