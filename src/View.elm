module View exposing (view)

import Date
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Model exposing (..)
import Msg exposing (..)
import Settings
import Util exposing (class)
import View.Budget
import View.Transactions


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

                SettingsPage ->
                    [ Element.map Msg.ChangeSettings (Settings.view model.settings) ]
    in
    layout [ class "fh" ]
        (column [ height fill, width fill ]
            [ el [ class "bm", width fill ] (navigation model)
            , el [ Border.widthXY 0 4, class "b-bh", width fill ]
                (column [ spacing 20, padding 20, centerX ] page)
            , el [ height fill, class "bm", width fill ] none
            ]
        )


navigation : Model -> Element Msg
navigation model =
    let
        label =
            Model.monthToDate model.currentMonth
                |> Date.format "MMMM y"

        activePage =
            [ class "bh fl" ]

        buttonStyle =
            [ height fill, width fill, Font.center, padding 10 ]
    in
    row [ spacing 10, width (fill |> maximum 1200), centerX, height (px 50), class "fm" ]
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
                (if model.currentPage == SettingsPage then
                    activePage

                 else
                    []
                )
                [ height fill, width (px 60), Font.center, Font.size 24 ]
            )
            { onPress = Just <| ChangePage SettingsPage
            , label = text "⚙"
            }
        ]
