module Main exposing (main)

import Browser
import Date
import Dict
import Model exposing (Model)
import Msg exposing (..)
import Set
import Task
import Time
import Update exposing (update)
import View exposing (view)


main =
    Browser.document
        { init = init, view = view, update = update, subscriptions = subscriptions }


init : {} -> ( Model, Cmd Msg )
init flags =
    ( { transactions = []
      , currentMonth = { month = Time.Jan, year = 0 }
      , budgetEntries = Dict.empty
      , newTransaction =
            { category = ""
            , value = ""
            , date = ""
            }
      }
    , Task.perform NewTime Time.now
    )


subscriptions model =
    Sub.none
