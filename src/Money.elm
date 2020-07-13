module Money exposing (..)

import Element
import Element.Input
import Html.Attributes
import Model exposing (Money)


parse : Money -> String -> Money
parse default val =
    case val of
        "" ->
            default

        "-" ->
            negate default

        _ ->
            let
                parsedInt =
                    val
                        |> String.replace "-" ""
                        |> String.replace "," ""
                        |> String.toInt
                        |> Maybe.withDefault default

                minuses =
                    List.length (String.indices "-" val)

                shouldNegate =
                    modBy 2 minuses == 1
            in
            if shouldNegate then
                negate parsedInt

            else
                parsedInt


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


input : List (Element.Attribute msg) -> InputOptions msg -> Element.Element msg
input attrs options =
    let
        padding =
            { top = 10, right = 0, bottom = 10, left = 10 }

        onChange =
            parse options.value >> options.onChange

        mergedAttrs =
            List.concat [ [ alignInput "right", Element.paddingEach padding ], attrs ]
    in
    Element.row [ Element.width Element.fill ]
        [ Element.Input.text mergedAttrs
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
        "green"

    else if val < 0 then
        "red"

    else
        ""
