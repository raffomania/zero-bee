module View exposing (view)

import Colors
import Date
import Element exposing (..)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Model exposing (..)
import Msg exposing (..)
import Settings
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
    layout
        [ Font.family
            [ Font.typeface "Open Sans"
            , Font.typeface "Helvetica Neue"
            , Font.typeface "Arial"
            , Font.sansSerif
            ]
        , Font.size 18
        ]
        (column
            [ height fill
            , width fill
            , padding 10
            , Element.Background.color (Element.rgb 0.97 0.97 0.97)
            ]
            [ el [ width fill ] (navigation model)
            , el
                [ width fill
                ]
                (column [ spacing 20, padding 20, centerX ] page)
            ]
        )


navigation : Model -> Element Msg
navigation model =
    let
        label =
            Model.monthToDate model.currentMonth
                |> Date.format "MMMM y"

        activePage =
            [ Border.widthEach
                { bottom = 2
                , top = 0
                , left = 0
                , right = 0
                }
            ]

        inactivePage =
            [ Font.color Colors.grey ]

        buttonStyle =
            [ height fill
            , width (maximum 200 fill)
            , Font.center
            , paddingEach { top = 20, bottom = 15, left = 15, right = 15 }
            ]

        buttonStyleForPage page =
            List.append
                buttonStyle
                (if model.currentPage == page then
                    activePage

                 else
                    inactivePage
                )
    in
    row
        [ spacing 45
        , width fill
        , paddingEach { top = 10, bottom = 0, left = 20, right = 20 }
        ]
        [ el [ centerY, Font.size 26, Font.color Colors.grey, Font.family [ Font.typeface "Input Mono" ], Font.italic ] (text "0🐝")
        , row [ height fill ]
            [ Input.button [ width (px 70), Font.center, height fill ]
                { onPress = Just PreviousMonth
                , label = text "<"
                }
            , el [ Font.center, width (px 200) ] (text label)
            , Input.button [ width (px 70), Font.center, height fill ]
                { onPress = Just NextMonth
                , label = text ">"
                }
            ]
        , row [ centerX, height fill, spacing 45 ]
            [ Input.button
                (buttonStyleForPage Budget)
                { onPress = Just <| ChangePage Budget
                , label = text "Budget"
                }
            , Input.button
                (buttonStyleForPage Transactions)
                { onPress = Just <| ChangePage Transactions
                , label = text "Transactions"
                }
            ]
        , Input.button
            (List.concat
                [ buttonStyleForPage SettingsPage
                , [ width (px 65), Font.size 24 ]
                ]
            )
            { onPress = Just <| ChangePage SettingsPage
            , label = text "⚙"
            }
        ]
