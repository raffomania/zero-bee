module Util exposing (..)

import Dict exposing (Dict)


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
