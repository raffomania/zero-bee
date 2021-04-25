module View.Autocomplete exposing (view)

import Element exposing (..)
import Element.Border exposing (roundEach)
import Element.Events
import Element.Input as Input
import Msg
import Util exposing (class)


type alias Model =
    { rawInput : String, label : String, availableValues : List String, focused : Bool }


view : Model -> Element Msg.Msg
view model =
    let
        isNew =
            List.isEmpty model.availableValues

        values =
            model.availableValues

        label =
            if isNew then
                model.label ++ " (new)"

            else
                model.label

        dropdownElement =
            if model.focused && not isNew then
                [ Element.below (suggestionList values) ]

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
        , text = model.rawInput
        , onChange = Msg.AddTransactionNewCategory
        }


suggestionList : List String -> Element Msg.Msg
suggestionList values =
    column
        [ class "bl"
        , paddingXY 20 10
        , roundEach { topLeft = 0, topRight = 0, bottomLeft = 3, bottomRight = 3 }
        , width fill
        , spacing 10
        ]
        (values
            |> List.map text
        )
