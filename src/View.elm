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
import View.Transactions


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
                    [ View.Transactions.view model ]

                Settings ->
                    [ Settings.view model ]
    in
    layout [ Background.color Colors.bg, Font.color Colors.black ]
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
