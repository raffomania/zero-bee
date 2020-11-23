module View.Transactions exposing (view)

import Date
import Dict
import Dict.Extra
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Model
import Money
import Msg
import Util exposing (class)


view model =
    column [ spacing 20 ]
        [ balance model
        , addTransactionForm model
        , transactionList model
        ]


addTransactionForm : Model.Model -> Element Msg.Msg
addTransactionForm model =
    row [ Util.onEnter Msg.AddTransaction, spacing 10 ]
        [ text "New transaction"
        , Money.input []
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
            , { header = text "Account", width = fill, view = \t -> text t.account }
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
            let
                val =
                    List.map .value ts
                        |> List.sum
                        |> Money.format model.settings.currencySymbol
            in
            text <| acc ++ ": " ++ val

        byAccount =
            model.transactions
                |> Dict.Extra.groupBy .account
                |> Dict.Extra.mapKeys (\acc -> if acc == "" then "default account" else acc)
                |> Dict.map transactionGroup
                |> Dict.values
    in
    column [spacing 10]
        ((text <| "Total Balance: " ++ total)
            :: byAccount
        )
