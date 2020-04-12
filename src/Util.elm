module Util exposing (..)

import Dict exposing (Dict)


dictUpsert : comparable -> (Maybe v -> Maybe v) -> v -> Dict comparable v -> Dict comparable v
dictUpsert key fn default dict =
    if Dict.member key dict then
        Dict.update key fn dict

    else
        Dict.insert key default dict
