module View exposing (view)

import Colors
import Date
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Model exposing (..)
import Money
import Msg exposing (..)
import Settings
import Util exposing (dictUpsert)


borders =
    { bottom = 0, left = 0, right = 0, top = 0 }


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
                    [ balance model
                    , addTransactionForm model
                    , transactionList model
                    ]

                Settings ->
                    [ Settings.view model ]
    in
    layout [ Background.color Colors.bg ]
        (column [ height fill, width fill ]
            [ el [ Background.color Colors.accent, width fill ] (navigation model)
            , el [ Border.widthXY 0 4, Border.color Colors.bgAccent, width fill ]
                (column [ spacing 20, padding 20, centerX ] page)
            , el [ height fill, Background.color Colors.accent, width fill ] none
            ]
        )


navigation : Model -> Element Msg
navigation model =
    let
        label =
            Model.monthToDate model.currentMonth
                |> Date.format "MMMM y"

        activePage =
            [ Background.color Colors.bgAccent ]

        buttonStyle =
            [ height fill, width fill, Font.center, padding 10 ]
    in
    row [ spacing 10, width (fill |> maximum 1200), centerX, height (px 50), Font.color Colors.bg ]
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
        , row [ width <| fillPortion 3, height fill ]
            [ Input.button
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
            , Input.button
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
            ]
        , Input.button
            (List.append
                (if model.currentPage == Settings then
                    activePage

                 else
                    []
                )
                [ height fill, width (px 60), Font.center, Font.size 24 ]
            )
            { onPress = Just <| ChangePage Settings
            , label = text "⚙"
            }
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
                |> List.filter (not << Model.isInFuture model.currentMonth)
                |> List.map .value
                |> List.filter ((<) 0)
                |> List.sum

        cashSpent =
            model.transactions
                |> List.filter (not << Model.isInFuture model.currentMonth)
                |> List.map .value
                |> List.filter ((>) 0)
                |> List.sum

        budgeted = previouslyBudgeted + currentlyBudgeted + budgetedInFuture

        overspent = min (previouslyBudgeted + currentlyBudgeted + cashSpent) 0
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
            , { value = overspent
              , text = "overspent"
              }
            , { value = availableCash - budgeted + overspent
              , text = "to be budgeted"
              }
            ]
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
            , currencySymbol = model.settings.currencySymbol
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
    let
        color date =
            if Date.compare model.date date == LT then
                Colors.grey

            else
                Colors.black
    in
    table [ spacing 10 ]
        { data =
            model.transactions
                |> List.filter (\t -> Model.dateToMonth t.date == model.currentMonth)
                |> List.sortWith (\a b -> Date.compare b.date a.date)
        , columns =
            [ { header = text "date"
              , width = fill
              , view = \t -> el [ Font.color <| color t.date ] (text <| Date.toIsoString t.date)
              }
            , { header = text "category"
              , width = fill
              , view = \t -> el [ Font.color <| color t.date ] <| text t.category
              }
            , { header = el [ Font.alignRight ] <| text "value"
              , width = fill
              , view =
                    \t ->
                        el [ Font.color <| Money.toColor t.value ]
                            (Money.input
                                { onChange = ChangeTransactionValue t
                                , value = t.value
                                , label = Nothing
                                , currencySymbol = model.settings.currencySymbol
                                }
                            )
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


balance model =
    let
        value =
            model.transactions
                |> List.filter (\t -> Date.compare t.date model.date /= GT)
                |> List.map .value
                |> List.sum
    in
    text <| "Balance: " ++ Money.format model.settings.currencySymbol value


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
                        Keyed.el []
                            ( r.category
                            , Money.input
                                { value = r.budgeted
                                , onChange = ChangeBudgetEntry model.currentMonth r.category
                                , label = Nothing
                                , currencySymbol = model.settings.currencySymbol
                                }
                            )
              }
            , { header = el [ Font.alignRight ] <| text "activity"
              , width = fill
              , view = \r -> el [ Font.alignRight, centerY ] <| text <| Money.format model.settings.currencySymbol r.activity
              }
            , { header = el [ Font.alignRight ] <| text "available"
              , width = fill
              , view = \r -> el [ Font.alignRight, centerY, Font.color (Money.toColor r.available) ] <| text <| Money.format model.settings.currencySymbol r.available
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
        |> List.filter (not << Model.isInFuture model.currentMonth)
        |> List.foldl (applyTransaction model.currentMonth) mergedRows
        |> Dict.values
        |> List.sortWith (Util.doubleCompare .activity (.budgeted >> negate))


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

        default =
            { category = transaction.category
            , budgeted = 0
            , activity = transaction.value
            , available = transaction.value
            }
    in
    dictUpsert
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
