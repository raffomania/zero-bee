module View.Transactions exposing (view)

import Colors
import Date
import Dict
import Dict.Extra
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (th)
import Model
import Model.Transactions
import Money
import Msg
import Util exposing (zeroPadding)
import View.Autocomplete


view : Model.Model -> Element Msg.Msg
view model =
    column [ spacing 20 ]
        [ balance model
        , column [ paddingXY 0 20, spacing 10, Border.color Colors.lightGrey, Border.widthEach { zeroPadding | bottom = 1 }, Border.solid ]
            [ text "Add transaction"
            , addTransactionForm model
            ]
        , transactionList model
        ]


addTransactionForm : Model.Model -> Element Msg.Msg
addTransactionForm model =
    row [ Util.onEnter Msg.AddTransaction, spacing 30 ]
        [ Money.input [ width (px 120) ]
            { value = model.newTransaction.value
            , onChange = Msg.AddTransactionNewValue
            , label = Just <| Input.labelAbove [] <| text "Value"
            , currencySymbol = model.settings.currencySymbol
            }
        , Input.text [ width (px 200) ]
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text ("Day in " ++ Date.format "MMMM y" (Model.monthToDate model.currentMonth))
            , text = model.newTransaction.dayOfMonth
            , onChange = Msg.AddTransactionNewDate
            }
        , View.Autocomplete.view
            { input = model.newTransaction.category
            , label = Just "Category"
            , availableValues = Model.autocompletedCategories model model.newTransaction.category
            , focused = model.newTransaction.focus == Just Model.Category
            , onChange = Msg.AddTransactionNewCategory
            , onChangeFocus = Msg.AddTransactionFocusCategoryInput
            , placeholder = Nothing
            }
        , Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "Note"
            , text = model.newTransaction.note
            , onChange = Msg.AddTransactionNewNote
            }
        , View.Autocomplete.view
            { input = model.newTransaction.account
            , label = Just "Account"
            , availableValues = Model.autocompletedAccounts model
            , focused = model.newTransaction.focus == Just Model.Account
            , onChange = Msg.AddTransactionNewAccount
            , onChangeFocus = Msg.AddTransactionFocusAccountInput
            , placeholder = Just <| Input.placeholder [] <| text "default"
            }
        ]


transactionList : Model.Model -> Element Msg.Msg
transactionList model =
    table [ spacingXY 30 30 ]
        { data =
            model.transactions
                |> List.filter (\t -> Model.dateToMonth t.date == model.currentMonth)
                |> List.sortWith (\a b -> Date.compare b.date a.date)
        , columns =
            -- The empty header works around this bug:
            -- https://github.com/mdgriffith/elm-ui/issues/161
            -- without it, the rows are reversed, breaking tab navigation direction
            [ { header = Element.text ""
              , width = px 120
              , view =
                    \t ->
                        el [ Font.color <| Money.toColor t.value ]
                            (Money.input [ Border.color Colors.lightGrey ]
                                { onChange = Msg.ChangeTransactionValue t
                                , value = t.value
                                , label = Nothing
                                , currencySymbol = model.settings.currencySymbol
                                }
                            )
              }
            , { header = none
              , width = px 200
              , view = \t -> el [ centerY ] (text <| Date.toIsoString t.date)
              }
            , { header = none
              , width = fill
              , view =
                    \transaction ->
                        let
                            isEditing =
                                (model.editingTransaction |> Maybe.map .transaction) == Just transaction

                            isFocused =
                                isEditing && (model.editingTransaction |> Maybe.map .focus) == Just Model.Category

                            input =
                                if isEditing then
                                    model.editingTransaction
                                        |> Maybe.map .input
                                        |> Maybe.withDefault transaction.category

                                else
                                    transaction.category
                        in
                        el [ centerY ] <|
                            View.Autocomplete.view
                                { placeholder = Nothing
                                , label = Nothing
                                , availableValues = Model.autocompletedCategories model input
                                , focused = isFocused
                                , onChangeFocus = Msg.ChangeEditingTransactionFocus transaction
                                , input = input
                                , onChange = Msg.ChangeEditingTransactionCategory transaction
                                }
              }
            , { header = none
              , width = fill

              -- TODO handle overly long notes here
              , view = \t -> el [ centerY ] <| text t.note
              }
            , { header = none
              , width = fill
              , view =
                    \t ->
                        el
                            [ centerY
                            , paddingEach { right = 40, left = 0, top = 0, bottom = 0 }
                            , onRight <|
                                Input.button [ height fill, Font.center, moveLeft 20 ]
                                    { onPress = Just <| Msg.RemoveTransaction t
                                    , label = text "x"
                                    }
                            ]
                            (t.account
                                |> Model.Transactions.mapAccountName
                                |> text
                            )
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

        transactionGroup _ ts =
            List.map .value ts
                |> List.sum
                |> Money.format model.settings.currencySymbol

        byAccount =
            model.transactions
                |> List.map (\t -> { t | account = Model.Transactions.mapAccountName (.account t) })
                |> Dict.Extra.groupBy .account
                |> Dict.map transactionGroup
                |> Dict.toList
                |> List.map (\( acc, val ) -> { value = val, text = acc })

        headerStyle =
            [ paddingXY 5 10
            , Border.widthEach
                { bottom = 1
                , left = 0
                , right = 0
                , top = 0
                }
            ]
    in
    table [ moveLeft 5 ]
        { data = byAccount
        , columns =
            [ { header = el (Font.alignRight :: headerStyle) <| text total
              , width = shrink
              , view = \d -> el [ Font.alignRight, padding 5 ] (text d.value)
              }
            , { header = el headerStyle <| text "Total Balance"
              , width = px 200
              , view = \d -> el [ padding 5 ] <| text d.text
              }
            ]
        }
