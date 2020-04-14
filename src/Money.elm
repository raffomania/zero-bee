module Money exposing (..)

import Model


parse : String -> Maybe Model.Money
parse val =
    case val of
        "" ->
            Just 0

        _ ->
            String.toInt val
