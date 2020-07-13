module View.Transactions exposing (view)
import Util
import Model
import Element.Input as Input
import Date
import Money
import Colors
import Element.Font as Font
import Msg

import Element exposing (..)

view model = column []
             [balance model
                , addTransactionForm model
                , transactionList model]


addTransactionForm : Model.Model -> Element Msg.Msg
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
    row [ Util.onEnter Msg.AddTransaction, spacing 10 ]
        [ text "New transaction"
        , Money.input []
            { value = model.newTransaction.value
            , onChange = Msg.AddTransactionNewValue
            , label = Just <| Input.labelAbove [] <| text "value"
            , currencySymbol = model.settings.currencySymbol
            }
        , Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "category"
            , text = model.newTransaction.category
            , onChange = Msg.AddTransactionNewCategory
            }
        , Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text ("date: " ++ currentDate)
            , text = model.newTransaction.date
            , onChange = Msg.AddTransactionNewDate
            }
        , Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "note"
            , text = model.newTransaction.note
            , onChange = Msg.AddTransactionNewNote
            }
        ]


transactionList : Model.Model -> Element Msg.Msg
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
            , { header = text "note"
              , width = fill
              , view = \t -> el [ Font.color <| color t.date ] <| text t.note
              }
            , { header = el [ Font.alignRight ] <| text "value"
              , width = fill
              , view =
                    \t ->
                        el [ Font.color <| Money.toColor t.value ]
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
        value =
            model.transactions
                |> List.filter (\t -> Date.compare t.date model.date /= GT)
                |> List.map .value
                |> List.sum
    in
    text <| "Balance: " ++ Money.format model.settings.currencySymbol value


