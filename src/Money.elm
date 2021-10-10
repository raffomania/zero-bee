module Money exposing (format, formatWithSign, input, toColor)

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


formatWithSign : String -> Money -> String
formatWithSign symbol val =
    let
        sign =
            if val > 0 then
                "+"

            else
                ""
    in
    sign ++ format symbol val


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
            { top = 10, right = 25, bottom = 10, left = 10 }

        onChange =
            parse options.value >> options.onChange

        currencyLabel =
            Element.onRight <|
                Element.el [ Element.moveLeft 20, Element.moveDown 9.5 ] <|
                    Element.text options.currencySymbol

        mergedAttrs =
            List.concat
                [ [ alignInput "right", Element.paddingEach padding, currencyLabel ]
                , attrs
                ]
    in
    Element.Input.text mergedAttrs
        { text = formatOnlyNumber options.value
        , placeholder = Nothing
        , label = options.label |> Maybe.withDefault (Element.Input.labelHidden "money value")
        , onChange = onChange
        }


alignInput : String -> Element.Attribute msg
alignInput val =
    Element.htmlAttribute (Html.Attributes.style "text-align" val)


toColor : Model.Money -> String
toColor val =
    if val > 0 then
        "green"

    else if val < 0 then
        "red"

    else
        ""
