module Money exposing (..)

import Element
import Element.Input
import Html.Attributes
import Model exposing (Money)


parse : String -> Maybe Money
parse val =
    case val of
        "" ->
            Just 0

        _ ->
            String.toInt val


format : Money -> String
format val =
    let
        str =
            String.fromInt val |> String.padRight 3 '0'

        wholes =
            String.slice 0 -2 str

        cents =
            String.slice -2 (String.length str) str
    in
    wholes ++ "," ++ cents ++ "€"


type alias InputOptions msg =
    { onChange : String -> msg
    , value : Money
    , label : Maybe (Element.Input.Label msg)
    }


input : InputOptions msg -> Element.Element msg
input options =
    let
        padding =
            { top = 10, right = 0, bottom = 10, left = 10 }
    in
    Element.row [Element.width Element.fill]
        [ Element.Input.text [ alignInput "right", Element.paddingEach padding ]
            { text = String.fromInt options.value
            , placeholder = Nothing
            , label = options.label |> Maybe.withDefault (Element.Input.labelHidden "money input")
            , onChange = options.onChange
            }
        , Element.text "€"
        ]


alignInput val =
    Element.htmlAttribute (Html.Attributes.style "text-align" val)
