module Money exposing (..)

import Colors
import Element
import Element.Font as Font
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


format : String -> Money -> String
format symbol val =
    formatOnlyNumber val ++ symbol


formatOnlyNumber : Money -> String
formatOnlyNumber val =
    let
        str =
            String.fromInt val |> String.padLeft 3 '0'

        wholes =
            String.slice 0 -2 str

        cents =
            String.slice -2 (String.length str) str
    in
    wholes ++ "," ++ cents


type alias InputOptions msg =
    { onChange : Money -> msg
    , value : Money
    , label : Maybe (Element.Input.Label msg)
    , currencySymbol : String
    }


parseInputValue : Money -> String -> Money
parseInputValue previous str =
    String.replace "," "" str
        |> String.toInt
        |> Maybe.withDefault previous


input : InputOptions msg -> Element.Element msg
input options =
    let
        padding =
            { top = 10, right = 0, bottom = 10, left = 10 }

        onChange =
            parseInputValue options.value >> options.onChange
    in
    Element.row [ Element.width Element.fill ]
        [ Element.Input.text [ alignInput "right", Element.paddingEach padding ]
            { text = formatOnlyNumber options.value
            , placeholder = Nothing
            , label = options.label |> Maybe.withDefault (Element.Input.labelHidden "money input")
            , onChange = onChange
            }
        , Element.text options.currencySymbol
        ]


alignInput val =
    Element.htmlAttribute (Html.Attributes.style "text-align" val)


toColor val =
    if val > 0 then
        Colors.green

    else if val < 0 then
        Colors.red

    else
        Colors.black
