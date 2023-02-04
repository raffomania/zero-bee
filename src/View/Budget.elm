module View.Budget exposing (calculateBudgetRows, view)

import Colors
import Dict exposing (Dict)
import Element exposing (..)
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import List.Extra
import Maybe.Extra
import Model exposing (..)
import Money
import Month
import Msg exposing (..)
import Util exposing (zeroPadding)


type alias BudgetRow =
    { category : String
    , budgeted : Money
    , activity : Money
    , available : Money
    }


view : Model -> Element Msg
view model =
    let
        ( activeBudgetRows, inactiveBudgetRows ) =
            calculateBudgetRows model
    in
    column [ spacing 20 ]
        (List.append
            [ toBeBudgeted model activeBudgetRows
            , entryTable model activeBudgetRows
            ]
            (inactiveCategories model inactiveBudgetRows)
        )


entryTable : Model -> List BudgetRow -> Element Msg
entryTable model budgetRows =
    table [ spacing 10 ]
        { data = budgetRows
        , columns =
            [ { header = text "category"
              , width = fillPortion 2
              , view =
                    \r ->
                        el [ centerY ] <| text r.category
              }
            , { header = el [ Font.alignRight ] <| text "budgeted"
              , width = fill
              , view = budgetInput model
              }
            , { header = el [ Font.alignRight ] <| text "activity"
              , width = fill
              , view = \r -> el [ Font.alignRight, centerY ] <| text <| Money.format model.settings.currencySymbol r.activity
              }
            , { header = el [ Font.alignRight ] <| text "available"
              , width = fill
              , view = \r -> el [ Font.alignRight, centerY, Font.color (Money.toColor r.available) ] <| text <| Money.format model.settings.currencySymbol r.available
              }
            ]
        }


budgetInput : Model -> BudgetRow -> Element Msg
budgetInput model r =
    let
        value =
            case model.editingBudgetEntry of
                Just entry ->
                    if entry.month == model.currentMonth && entry.category == r.category then
                        entry.value

                    else
                        r.budgeted

                Nothing ->
                    r.budgeted
    in
    Keyed.el []
        ( r.category
          -- TODO update this on every keypress as sorting is not based on budgeted amount anymore
        , Money.input
            [ Events.onLoseFocus (Msg.ChangeBudgetEntry model.currentMonth r.category value)
            , Util.onEnter (Msg.ChangeBudgetEntry model.currentMonth r.category value)
            ]
            { value = value
            , onChange = Msg.ChangeEditedBudgetEntry model.currentMonth r.category
            , label = Nothing
            , currencySymbol = model.settings.currencySymbol
            }
        )


{-| Create a view model for displaying two tables of active and inactive budget entries.
-}
calculateBudgetRows : Model -> ( List BudgetRow, List BudgetRow )
calculateBudgetRows model =
    let
        pastMonths =
            model.budgetEntries
                |> Dict.filter (\i _ -> compareMonths model.currentMonth (parseMonthIndex i) == GT)
                |> Dict.map (\_ monthDict -> Dict.map (\_ e -> budgetRowFromEntry e) monthDict)
                |> Dict.foldl applyMonthDict Dict.empty

        thisMonth =
            model.budgetEntries
                |> Dict.get (getMonthIndex model.currentMonth)
                |> Maybe.withDefault Dict.empty
                |> Dict.map (\_ e -> budgetRowFromEntry e)

        mergedRows =
            Dict.merge
                (\k past -> Dict.insert k { past | budgeted = 0 })
                (\k past present -> Dict.insert k { present | available = past.available + present.available })
                (\k present -> Dict.insert k present)
                pastMonths
                thisMonth
                Dict.empty

        pastTwoMonthsTransactions =
            model.transactions
                |> List.filter (\t -> Model.compareMonths (Model.dateToMonth t.date) (Month.decrement <| Month.decrement model.currentMonth) == GT)

        isRowActive r =
            r.activity
                /= 0
                || r.available
                /= 0
                || r.budgeted
                /= 0
                || (pastTwoMonthsTransactions
                        |> List.Extra.find (\t -> t.category == r.category)
                        |> Maybe.Extra.isJust
                   )
    in
    model.transactions
        |> List.filter (not << Model.isInFuture model.currentMonth)
        |> List.foldl (applyTransaction model.currentMonth) mergedRows
        |> Dict.values
        |> List.sortBy .category
        |> List.partition isRowActive


applyMonthDict : MonthIndex -> Dict MonthIndex BudgetRow -> Dict CategoryId BudgetRow -> Dict CategoryId BudgetRow
applyMonthDict _ monthDict rowDict =
    let
        updateRow : BudgetRow -> BudgetRow -> BudgetRow
        updateRow newRow oldRow =
            { oldRow | budgeted = oldRow.budgeted + newRow.budgeted, available = oldRow.available + newRow.available }
    in
    monthDict
        |> Dict.foldl (\category row -> Util.dictUpsert category (updateRow row) row) rowDict


applyTransaction : MonthOfYear -> Transaction -> Dict CategoryId BudgetRow -> Dict CategoryId BudgetRow
applyTransaction currentMonth transaction rows =
    let
        activityValue =
            if dateToMonth transaction.date == currentMonth then
                transaction.value

            else
                0

        updateRow =
            \r -> { r | activity = r.activity + activityValue, available = r.available + transaction.value }

        default =
            { category = transaction.category
            , budgeted = 0
            , activity = activityValue
            , available = transaction.value
            }
    in
    Util.dictUpsert
        transaction.category
        updateRow
        default
        rows


budgetRowFromEntry : BudgetEntry -> BudgetRow
budgetRowFromEntry entry =
    { category = entry.category
    , budgeted = entry.value
    , activity = 0
    , available = entry.value
    }


toBeBudgeted : Model -> List BudgetRow -> Element Msg
toBeBudgeted model budgetRows =
    let
        sumBudgets dict =
            Dict.values dict
                |> List.map .value
                |> List.sum

        -- todo this should probably include past overspent values
        previouslyBudgeted =
            model.budgetEntries
                |> Dict.filter (\index _ -> compareMonths (parseMonthIndex index) model.currentMonth == LT)
                |> Dict.values
                |> List.map sumBudgets
                |> List.sum

        currentlyBudgeted =
            model.budgetEntries
                |> Dict.get (getMonthIndex model.currentMonth)
                |> Maybe.withDefault Dict.empty
                |> sumBudgets

        budgetedInFuture =
            model.budgetEntries
                |> Dict.filter (\index _ -> compareMonths (parseMonthIndex index) model.currentMonth == GT)
                |> Dict.values
                |> List.map sumBudgets
                |> List.sum

        totalBudgeted =
            previouslyBudgeted + currentlyBudgeted + budgetedInFuture

        overspent =
            budgetRows
                |> List.map .available
                |> List.filter ((>) 0)
                |> List.sum

        -- totalAvailable =
        --     budgetRows
        --         |> List.map .available
        --         |> List.sum
        --
        -- positiveActivity =
        --     model.transactions
        --         |> List.filter (Model.isTransactionInMonth model.currentMonth)
        --         |> List.map .value
        --         |> List.filter ((<) 0)
        --         |> List.sum
        --
        -- negativeActivity =
        --     model.transactions
        --         |> List.filter (Model.isTransactionInMonth model.currentMonth)
        --         |> List.map .value
        --         |> List.filter ((>) 0)
        --         |> List.sum
        --
        markedInflow =
            budgetRows
                |> List.map .budgeted
                |> List.filter ((>) 0)
                |> List.sum

        plannedOutflow =
            budgetRows
                |> List.map .budgeted
                |> List.filter ((<) 0)
                |> List.sum

        data =
            [ -- { value = totalAvailable, text = "total available" }
              { value = -previouslyBudgeted, text = "from previous month" }

            -- , { value = inflow, text = "positive activity" }
            -- , { value = outflow, text = "negative activity"}
            , { value = -markedInflow, text = "marked as inflow" }
            , { value = -plannedOutflow
              , text = "budgeted this month"
              }
            , { value = -budgetedInFuture
              , text = "planned in future"
              }
            , { value = overspent
              , text = "overspent"
              }
            , { value = negate totalBudgeted + overspent
              , text = "to be budgeted"
              }
            ]
    in
    table [ spacing 10, paddingXY 0 30 ]
        { data = data
        , columns =
            [ { header = none
              , width = shrink
              , view = \d -> el [ Font.alignRight ] <| text <| Money.formatWithSign model.settings.currencySymbol d.value
              }
            , { header = none
              , width = fill
              , view = \d -> text d.text
              }
            ]
        }


inactiveCategories : Model -> List BudgetRow -> List (Element Msg)
inactiveCategories model inactiveBudgetRows =
    if List.isEmpty inactiveBudgetRows then
        []

    else
        [ el [ Font.color Colors.grey, paddingEach { zeroPadding | top = 30 } ] (text "Inactive entries")
        , entryTable model inactiveBudgetRows
        ]
