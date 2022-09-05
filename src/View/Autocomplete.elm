module View.Autocomplete exposing (view)

import Colors
import Element exposing (..)
import Element.Background
import Element.Border exposing (rounded)
import Element.Events
import Element.Input as Input
import Msg


type alias Model =
    { input : String
    , label : Maybe String
    , availableValues : List String
    , focused : Bool
    , onChange : String -> Msg.Msg
    , onChangeFocus : Bool -> Msg.Msg
    , placeholder : Maybe (Input.Placeholder Msg.Msg)
    }


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
                Maybe.map (\l -> l ++ " (new)") model.label

            else
                model.label

        labelElement =
            case label of
                Just str ->
                    Input.labelAbove [] <| text str

                Nothing ->
                    Input.labelHidden "Autocomplete"

        dropdownElement =
            if model.focused && not isNew && not (List.isEmpty suggestionValues) then
                [ Element.below (suggestionList suggestionValues) ]

            else
                []
    in
    Input.text
        (List.concat
            [ dropdownElement
            , [ Element.Events.onFocus (model.onChangeFocus True)
              , Element.Events.onLoseFocus (model.onChangeFocus False)
              ]
            ]
        )
        { placeholder = model.placeholder
        , label = labelElement
        , text = model.input
        , onChange = model.onChange
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
