module View exposing (view)

import Browser
import Date
import Dict exposing (Dict)
import Element exposing (Element, column, el, row, text)
import Element.Input as Input
import Html.Events
import Json.Decode as Decode
import Model exposing (..)
import Msg exposing (..)


view : Model -> Browser.Document Msg
view model =
    { title = "whyNAB"
    , body =
        [ Element.layout [ Element.padding 20 ]
            (column [ Element.spacing 10 ]
                [ addTransactionForm model
                , monthPicker model
                , transactionList model
                , budgetView model
                ]
            )
        ]
    }


monthPicker : Model -> Element Msg
monthPicker model =
    let
        label =
            Debug.toString model.currentMonth.month ++ " " ++ String.fromInt model.currentMonth.year
    in
    text label


addTransactionForm : Model -> Element Msg
addTransactionForm model =
    let
        currentDate =
            Debug.toString <| Result.map Date.toIsoString <| Date.fromIsoString model.newTransaction.date
    in
    row [ onEnter AddTransaction, Element.spacing 10 ]
        [ text "New transaction"
        , Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "value"
            , text = model.newTransaction.value
            , onChange = AddTransactionNewValue
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
    Element.table []
        { data = model.transactions
        , columns =
            [ { header = text "date"
              , width = Element.fill
              , view = \t -> text <| Date.toIsoString t.date
              }
            , { header = text "category"
              , width = Element.fill
              , view = \t -> text <| t.category
              }
            , { header = text "category"
              , width = Element.fill
              , view = \t -> text <| String.fromInt t.value
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
    Element.table []
        { data = budgetRows model
        , columns =
            [ { header = text "category"
              , width = Element.fill
              , view = \r -> text r.category
              }
            , { header = text "budgeted"
              , width = Element.fill
              , view = \r -> text <| String.fromInt r.budgeted
              }
            , { header = text "activity"
              , width = Element.fill
              , view = \r -> text <| String.fromInt r.activity
              }
            , { header = text "available"
              , width = Element.fill
              , view = \r -> text <| String.fromInt r.available
              }
            ]
        }


budgetRows : Model -> List BudgetRow
budgetRows model =
    model.transactions
        |> List.foldl (updateBudgetRowDict model) Dict.empty
        |> Dict.values


updateBudgetRowDict : Model -> Transaction -> Dict CategoryId BudgetRow -> Dict CategoryId BudgetRow
updateBudgetRowDict model transaction rows =
    if Dict.member transaction.category rows then
        let
            updateRow =
                \r -> { r | activity = r.activity + transaction.value, available = r.available + transaction.value }
        in
        Dict.update transaction.category (Maybe.map updateRow) rows

    else
        let
            budget =
                budgetEntry model transaction.category
        in
        Dict.insert transaction.category { category = transaction.category, budgeted = budget, activity = transaction.value, available = budget + transaction.value } rows


budgetEntry : Model -> CategoryId -> Money
budgetEntry model name =
    let
        entry =
            Model.getBudgetEntry name model.currentMonth model
                |> Maybe.withDefault
                    { month = model.currentMonth
                    , value = 0
                    , category = name
                    }
    in
    entry.value


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
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
