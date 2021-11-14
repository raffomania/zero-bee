module Model.Transactions exposing (..)


mapAccountName : String -> String
mapAccountName name =
    if name == "" then
        "default"

    else
        name
