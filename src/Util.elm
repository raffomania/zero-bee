module Util exposing (..)

import Dict exposing (Dict)
import Element
import Html.Events
import Json.Decode as Decode


dictUpsert : comparable -> (v -> v) -> v -> Dict comparable v -> Dict comparable v
dictUpsert key fn default dict =
    if Dict.member key dict then
        Dict.update key (Maybe.map fn) dict

    else
        Dict.insert key default dict


doubleCompare : (v -> comparable) -> (v -> comparable) -> v -> v -> Order
doubleCompare fnFirst fnSecond aVal bVal =
    let
        firstOrder =
            compare (fnFirst aVal) (fnFirst bVal)
    in
    if firstOrder /= EQ then
        firstOrder

    else
        compare (fnSecond aVal) (fnSecond bVal)

onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )

