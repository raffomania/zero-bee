module View exposing (view)

import Colors
import Date
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Model exposing (..)
import Money
import Msg exposing (..)
import Settings
import Util exposing (dictUpsert)
import View.Budget


borders =
    { bottom = 0, left = 0, right = 0, top = 0 }


view : Model -> Html.Html Msg
view model =
    let
        page =
            case model.currentPage of
                Budget ->
                    [ View.Budget.view model
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
    row [ Util.onEnter AddTransaction, spacing 10 ]
        [ text "New transaction"
        , Money.input []
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
                            (Money.input []
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


alignInput val =
    htmlAttribute (Html.Attributes.style "text-align" val)
