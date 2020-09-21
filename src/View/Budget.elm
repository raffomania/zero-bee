module View.Budget exposing (view)

import Date
import Dict exposing (Dict)
import Element exposing (..)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Model exposing (..)
import Money
import Msg exposing (..)
import Util


type alias BudgetRow =
    { category : String
    , budgeted : Money
    , activity : Money
    , available : Money
    }


view : Model -> Element Msg
view model =
    let
        budgetRows =
            calculateBudgetRows model
    in
    column [ spacing 20 ]
        [ toBeBudgeted model budgetRows
        , entryTable model budgetRows
        ]


entryTable : Model -> List BudgetRow -> Element Msg
entryTable model budgetRows =
    table [ spacing 10 ]
        { data = budgetRows
        , columns =
            [ { header = text "category"
              , width = fillPortion 2
              , view =
                    \r ->
                        let
                            attrs =
                                List.append
                                    [ centerY ]
                                    (if r.activity == 0 then
                                        [ Util.class "fl" ]

                                     else
                                        []
                                    )
                        in
                        el attrs <| text r.category
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
              , view = \r -> el [ Font.alignRight, centerY, Util.class (Money.toColor r.available) ] <| text <| Money.format model.settings.currencySymbol r.available
              }
            , { header = none
              , width = px 50
              , view =
                    \r ->
                        if r.activity /= 0 then
                            none

                        else
                            Input.button [ Font.center, height fill ]
                                { onPress = Just <| RemoveBudgetEntry model.currentMonth r.category
                                , label = text "x"
                                }
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


calculateBudgetRows : Model -> List BudgetRow
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
    in
    model.transactions
        |> List.filter (not << Model.isInFuture model.currentMonth)
        |> List.foldl (applyTransaction model.currentMonth) mergedRows
        |> Dict.values
        |> List.sortWith (Util.doubleCompare .activity (.budgeted >> negate))


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
            , activity = transaction.value
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

        previouslyBudgeted =
            model.budgetEntries
                |> Dict.filter (\index _ -> (compareMonths (parseMonthIndex index) model.currentMonth) == LT)
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

        availableCash =
            model.transactions
                |> List.filter (not << Model.isInFuture model.currentMonth)
                |> List.map .value
                |> List.filter ((<) 0)
                |> List.sum

        budgeted =
            previouslyBudgeted + currentlyBudgeted + budgetedInFuture

        overspent =
            budgetRows
                |> List.map .available
                |> List.filter ((>) 0)
                |> List.sum

        balance =
            model.transactions
                |> List.filter (not << Model.isInFuture model.currentMonth)
                |> List.map .value
                |> List.sum

        totalAvailable =
            budgetRows
                |> List.map .available
                |> List.sum

        data =
            [ { value = availableCash - previouslyBudgeted
              , text = "funds for " ++ Date.format "MMMM" (Model.monthToDate model.currentMonth)
              }
            , { value = balance, text = "balance" }
            , { value = totalAvailable, text = "total available" }
            , { value = previouslyBudgeted, text = "previously budgeted"}
            , { value = currentlyBudgeted
              , text = "budgeted"
              }
            , { value = budgetedInFuture
              , text = "budgeted in future"
              }
            , { value = overspent
              , text = "overspent"
              }
            , { value = negate budgeted + overspent
              , text = "to be budgeted"
              }
            ]
    in
    table [ spacing 10 ]
        { data = data
        , columns =
            [ { header = none
              , width = px 200
              , view = \d -> el [ Font.alignRight ] <| text <| Money.format model.settings.currencySymbol d.value
              }
            , { header = none
              , width = px 200
              , view = \d -> text <| d.text
              }
            ]
        }
