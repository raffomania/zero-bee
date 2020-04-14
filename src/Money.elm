module Money exposing (..)

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
    let str = String.fromInt val |> String.padRight 3 '0'
        wholes = String.slice 0 -2 str
        cents = String.slice -2 (String.length str) str
    in
     wholes ++ "," ++ cents ++ "€"
