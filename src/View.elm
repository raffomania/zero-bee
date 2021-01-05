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
    layout [ class "fh bm" ]
        (column [ height fill, width fill ]
            [ el [ class "bm", width fill ] (navigation model)
            , el
                [ Border.widthXY 0 4
                , class "b-bh bg"
                , width fill
                , Border.shadow
                    { offset = ( 0, 10 )
                    , size = -10
                    , blur = 10
                    , color = rgba 0 0 0 0.2
                    }
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
            [ class "bh fl"
            , Border.shadow
                { offset = ( 0, 0 )
                , size = 0
                , blur = 10
                , color = rgba 0 0 0 0.3
                }
            ]

        inactivePage =
            [ Border.shadow
                { offset = ( 0, 0 )
                , size = 1
                , blur = 10
                , color = rgba 0 0 0 0.2
                }
            ]

        buttonStyle =
            [ height fill
            , width fill
            , Font.center
            , paddingEach { top = 20, bottom = 15, left = 15, right = 15 }
            , Border.roundEach { topLeft = 4, topRight = 4, bottomLeft = 0, bottomRight = 0 }
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
        [ spacing 20
        , width fill
        , centerX
        , class "fm"
        , paddingEach { top = 10, bottom = 0, left = 10, right = 10 }
        , Border.innerShadow
            { offset = ( 0, -15 )
            , size = -15
            , blur = 15
            , color = rgba 0 0 0 0.2
            }
        ]
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
        , row [ width <| fillPortion 3, height fill, spacing 10 ]
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
                , [ width (px 60), Font.size 24 ]
                ]
            )
            { onPress = Just <| ChangePage SettingsPage
            , label = text "⚙"
            }
        ]
