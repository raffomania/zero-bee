module View.Autocomplete exposing (view)

import Element exposing (..)
import Element.Background
import Element.Border exposing (rounded)
import Element.Events
import Element.Input as Input
import Msg
import Util exposing (class)


type alias Model =
    { input : String, label : String, availableValues : List String, focused : Bool }


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
        , text = model.input
        , onChange = Msg.AddTransactionNewCategory
        }


suggestionList : List String -> Element Msg.Msg
suggestionList values =
    column
        [ class "bl"
        , paddingXY 20 20
        , rounded 5
        , width fill
        , spacing 25
        , Element.Background.color (rgb 1 1 1)
        , moveDown 10
        ]
        (values
            |> List.map text
        )
