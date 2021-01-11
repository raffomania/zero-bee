module View.Transactions exposing (view)

import Date
import Dict
import Dict.Extra
import Element exposing (..)
import Element.Font as Font
import Element.Border as Border
import Element.Input as Input
import Model
import Model.Transactions
import Money
import Msg
import Util exposing (class)


view model =
    column [ spacing 20 ]
        [ balance model
        , column [paddingXY 0 30, spacing 10]
            [
                text "Add transaction:",
                addTransactionForm model
            ]
        , transactionList model
        ]


addTransactionForm : Model.Model -> Element Msg.Msg
addTransactionForm model =
    row [ Util.onEnter Msg.AddTransaction, spacing 10 ]
        [ Money.input []
            { value = model.newTransaction.value
            , onChange = Msg.AddTransactionNewValue
            , label = Just <| Input.labelAbove [] <| text "Value"
            , currencySymbol = model.settings.currencySymbol
            }
        , Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "Category"
            , text = model.newTransaction.category
            , onChange = Msg.AddTransactionNewCategory
            }
        , Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text ("Day in " ++ Date.format "MMMM y" (Model.monthToDate model.currentMonth) ++ ":")
            , text = model.newTransaction.dayOfMonth
            , onChange = Msg.AddTransactionNewDate
            }
        , Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "Note"
            , text = model.newTransaction.note
            , onChange = Msg.AddTransactionNewNote
            }
        , Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "Account"
            , text = model.newTransaction.account
            , onChange = Msg.AddTransactionNewAccount
            }
        ]


transactionList : Model.Model -> Element Msg.Msg
transactionList model =
    let
        color date =
            if Date.compare model.date date == LT then
                "fl"

            else
                "fh"
    in
    table [ spacing 10 ]
        { data =
            model.transactions
                |> List.filter (\t -> Model.dateToMonth t.date == model.currentMonth)
                |> List.sortWith (\a b -> Date.compare b.date a.date)
        , columns =
            [ { header = text "Date"
              , width = fill
              , view = \t -> el [ class <| color t.date ] (text <| Date.toIsoString t.date)
              }
            , { header = text "Account"
              , width = fill
              , view = .account >> Model.Transactions.mapAccountName >> text
              }
            , { header = text "Category"
              , width = fill
              , view = \t -> el [ class <| color t.date ] <| text t.category
              }
            , { header = text "Note"
              , width = fill
              , view = \t -> el [ class <| color t.date ] <| text t.note
              }
            , { header = el [ Font.alignRight ] <| text "Value"
              , width = fill
              , view =
                    \t ->
                        el [ class <| Money.toColor t.value ]
                            (Money.input []
                                { onChange = Msg.ChangeTransactionValue t
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
                            { onPress = Just <| Msg.RemoveTransaction t
                            , label = text "x"
                            }
              }
            ]
        }


balance : Model.Model -> Element Msg.Msg
balance model =
    let
        total =
            model.transactions
                |> List.filter (\t -> Date.compare t.date model.date /= GT)
                |> List.map .value
                |> List.sum
                |> Money.format model.settings.currencySymbol

        transactionGroup acc ts =
            List.map .value ts
                |> List.sum
                |> Money.format model.settings.currencySymbol

        byAccount =
            model.transactions
                |> List.map (\t -> {t | account = Model.Transactions.mapAccountName (.account t)})
                |> Dict.Extra.groupBy .account
                |> Dict.map transactionGroup
                |> Dict.toList
                |> List.map (\( acc, val ) -> { value = val, text = acc })

        headerStyle = 
                [paddingXY 5 10, Border.widthEach {
                    bottom = 1,
                    left = 0,
                    right = 0,
                    top = 0
                }]
    in
    table [ moveLeft 5]
        { data = byAccount
        , columns =
            [ { header = el (Font.alignRight :: headerStyle ) <| text total
              , width = shrink
              , view = \d -> el [Font.alignRight, padding 5] (text d.value)
              }
            , { header =  el headerStyle <| text "Total Balance"
              , width = px 200
              , view = \d -> el [padding 5] <| text d.text
              }
            ]
        }
