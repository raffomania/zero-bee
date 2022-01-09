module View.Autocomplete exposing (view)

import Colors
import Element exposing (..)
import Element.Background
import Element.Border exposing (rounded)
import Element.Events
import Element.Input as Input
import Msg


type alias Model =
    { input : String, label : String, availableValues : List String, focused : Bool }


view : Model -> Element Msg.Msg
view model =
    let
        isNew =
            List.isEmpty model.availableValues

        suggestionValues =
            model.availableValues
                |> List.filter (\t -> t /= "")

        label =
            if isNew then
                model.label ++ " (new)"

            else
                model.label

        dropdownElement =
            if model.focused && not isNew && not (List.isEmpty suggestionValues) then
                [ Element.below (suggestionList suggestionValues) ]

            else
                []
    in
    Input.text
        (List.concat
            [ dropdownElement
            , [ Element.Events.onFocus (Msg.AddTransactionFocusCategoryInput True)
              , Element.Events.onLoseFocus (Msg.AddTransactionFocusCategoryInput False)
              ]
            ]
        )
        { placeholder = Nothing
        , label = Input.labelAbove [] <| text label
        , text = model.input
        , onChange = Msg.AddTransactionNewCategory
        }


suggestionList : List String -> Element Msg.Msg
suggestionList values =
    column
        [ paddingXY 20 20
        , rounded 5
        , spacing 25
        , Element.Background.color Colors.lightGrey
        , moveDown 10
        ]
        (values
            |> List.map text
        )
