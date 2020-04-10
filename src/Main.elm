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
      , categories = Set.empty
      , currentMonth = { month = Time.Jan, year = 0 }
      , budgetEntries = Dict.empty
      , newCategory = ""
      , newTransaction =
            { category = ""
            , value = 0
            , date = Date.fromPosix Time.utc (Time.millisToPosix 0)
            }
      }
    , Task.perform NewTime Time.now
    )


subscriptions model =
    Sub.none
