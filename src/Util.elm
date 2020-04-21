module Util exposing (..)

import Dict exposing (Dict)


dictUpsert : comparable -> (v -> v) -> v -> Dict comparable v -> Dict comparable v
dictUpsert key fn default dict =
    if Dict.member key dict then
        Dict.update key (Maybe.map fn) dict

    else
        Dict.insert key default dict
