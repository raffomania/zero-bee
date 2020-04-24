module View exposing (view)

import Browser
import Colors
import Date
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Model exposing (..)
import Money
import Month
import Msg exposing (..)
import Util exposing (dictUpsert)


view : Model -> Html.Html Msg
view model =
    let
        page =
            case model.currentPage of
                Budget ->
                    [ toBeBudgeted model
                    , budgetView model
                    ]

                Transactions ->
                    [ addTransactionForm model
                    , transactionList model
                    ]
    in
    layout [ Background.color Colors.bg ]
        (column []
            [ navigation model
            , column [ spacing 20, padding 20 ] page
            ]
        )


navigation : Model -> Element Msg
navigation model =
    let
        label =
            Model.monthToDate model.currentMonth
                |> Date.format "MMMM y"

        activePage =
            [ Background.color Colors.accent ]

        buttonStyle =
            [ height fill, padding 10 ]
    in
    row [ spacing 10, Background.color Colors.bgAccent, width fill, height (px 50) ]
        [ row [ width <| fillPortion 2, height fill, spacing 10 ]
            [ Input.button [ width fill, Font.center, height fill ]
                { onPress = Just PreviousMonth
                , label = text "<"
                }
            , el [ Font.center, width (px 150) ] (text label)
            , Input.button [ width fill, Font.center, height fill ]
                { onPress = Just NextMonth
                , label = text ">"
                }
            ]
        , row [width <| fillPortion 3, height fill]
            [ Input.button
                (List.append
                    (if model.currentPage == Transactions then
                        activePage

                     else
                        []
                    )
                    buttonStyle
                )
                { onPress = Just <| ChangePage Transactions
                , label = text "Transactions"
                }
            , Input.button
                (List.append
                    (if model.currentPage == Budget then
                        activePage

                     else
                        []
                    )
                    buttonStyle
                )
                { onPress = Just <| ChangePage Budget
                , label = text "Budget"
                }
            ]
        ]


toBeBudgeted : Model -> Element Msg
toBeBudgeted model =
    let
        currentMonthEntries =
            Dict.get (getMonthIndex model.currentMonth) model.budgetEntries |> Maybe.withDefault Dict.empty

        sumBudgets dict =
            Dict.values dict
                |> List.map .value
                |> List.sum

        previouslyBudgeted =
            model.budgetEntries
                |> Dict.filter (\index _ -> compareMonths (parseMonthIndex index) model.currentMonth == LT)
                |> Dict.values
                |> List.map sumBudgets
                |> List.sum

        currentlyBudgeted =
            sumBudgets currentMonthEntries

        budgetedInFuture =
            model.budgetEntries
                |> Dict.filter (\index _ -> compareMonths (parseMonthIndex index) model.currentMonth == GT)
                |> Dict.values
                |> List.map sumBudgets
                |> List.sum

        availableCash =
            model.transactions
                |> List.filter
                    (\t -> Model.compareMonths model.currentMonth (Model.dateToMonth t.date) /= LT)
                |> List.map .value
                |> List.filter ((<) 0)
                |> List.sum
    in
    table [ spacing 10 ]
        { data =
            [ { value = availableCash
              , text = "funds"
              }
            , { value = -previouslyBudgeted
              , text = "previously budgeted"
              }
            , { value = -currentlyBudgeted
              , text = "budgeted"
              }
            , { value = -budgetedInFuture
              , text = "budgeted in future"
              }
            , { value = availableCash - previouslyBudgeted - currentlyBudgeted - budgetedInFuture
              , text = "to be budgeted"
              }
            ]
        , columns =
            [ { header = none
              , width = px 200
              , view = \d -> el [ Font.alignRight ] <| text <| Money.format d.value
              }
            , { header = none
              , width = px 200
              , view = \d -> text <| d.text
              }
            ]
        }


addTransactionForm : Model -> Element Msg
addTransactionForm model =
    let
        parsedDate =
            Result.map Date.toIsoString <| Date.fromIsoString model.newTransaction.date

        currentDate =
            case parsedDate of
                Ok d ->
                    d

                _ ->
                    "Invalid date"
    in
    row [ onEnter AddTransaction, spacing 10 ]
        [ text "New transaction"
        , Money.input
            { value = model.newTransaction.value
            , onChange = AddTransactionNewValue
            , label = Just <| Input.labelAbove [] <| text "value"
            }
        , Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "category"
            , text = model.newTransaction.category
            , onChange = AddTransactionNewCategory
            }
        , Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text ("date: " ++ currentDate)
            , text = model.newTransaction.date
            , onChange = AddTransactionNewDate
            }
        ]


transactionList model =
    table [ spacing 10 ]
        { data =
            model.transactions
                |> List.filter (\t -> Model.dateToMonth t.date == model.currentMonth)
                |> List.sortWith (\a b -> Date.compare b.date a.date)
        , columns =
            [ { header = text "date"
              , width = fill
              , view = \t -> text <| Date.toIsoString t.date
              }
            , { header = text "category"
              , width = fill
              , view = \t -> text <| t.category
              }
            , { header = el [ Font.alignRight ] <| text "value"
              , width = fill
              , view =
                    \t ->
                        Money.input
                            { onChange = ChangeTransactionValue t
                            , value = t.value
                            , label = Nothing
                            }
              }
            , { header = none
              , width = px 40
              , view =
                    \t ->
                        Input.button [ height fill, Font.center ]
                            { onPress = Just <| RemoveTransaction t
                            , label = text "x"
                            }
              }
            ]
        }


type alias BudgetRow =
    { category : String
    , budgeted : Money
    , activity : Money
    , available : Money
    }


budgetView : Model -> Element Msg
budgetView model =
    table [ spacing 10 ]
        { data = budgetRows model
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
                                        [ Font.color Colors.grey ]

                                     else
                                        []
                                    )
                        in
                        el attrs <| text r.category
              }
            , { header = el [ Font.alignRight ] <| text "budgeted"
              , width = fill
              , view =
                    \r ->
                        Money.input
                            { value = r.budgeted
                            , onChange = ChangeBudgetEntry model.currentMonth r.category
                            , label = Nothing
                            }
              }
            , { header = el [ Font.alignRight ] <| text "activity"
              , width = fill
              , view = \r -> el [ Font.alignRight, centerY ] <| text <| Money.format r.activity
              }
            , { header = el [ Font.alignRight ] <| text "available"
              , width = fill
              , view = \r -> el [ Font.alignRight, centerY ] <| text <| Money.format r.available
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


budgetRows : Model -> List BudgetRow
budgetRows model =
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
        |> List.filter (\t -> Model.compareMonths model.currentMonth (Model.dateToMonth t.date) /= LT)
        |> List.foldl (applyTransaction model.currentMonth) mergedRows
        |> Dict.values


applyMonthDict : MonthIndex -> Dict MonthIndex BudgetRow -> Dict CategoryId BudgetRow -> Dict CategoryId BudgetRow
applyMonthDict monthIndex monthDict rowDict =
    let
        updateRow : BudgetRow -> BudgetRow -> BudgetRow
        updateRow newRow oldRow =
            { oldRow | budgeted = oldRow.budgeted + newRow.budgeted, available = oldRow.available + newRow.available }
    in
    monthDict
        |> Dict.foldl (\category row -> dictUpsert category (updateRow row) row) rowDict


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
    in
    dictUpsert
        transaction.category
        updateRow
        { category = transaction.category
        , budgeted = 0
        , activity = transaction.value
        , available = transaction.value
        }
        rows


budgetRowFromEntry : BudgetEntry -> BudgetRow
budgetRowFromEntry entry =
    { category = entry.category
    , budgeted = entry.value
    , activity = 0
    , available = entry.value
    }


onEnter : msg -> Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


alignInput val =
    htmlAttribute (Html.Attributes.style "text-align" val)
