module Main exposing (main)

import Browser
import Date
import Dict
import Model exposing (Model)
import Msg exposing (..)
import Storage
import Task
import Time
import Update exposing (update)
import View exposing (view)


main : Program {} Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { transactions = []
      , currentMonth = { month = Time.Jan, year = 0 }
      , budgetEntries = Dict.empty
      , editingBudgetEntry = Nothing
      , currentPage = Model.Budget
      , date = Date.fromPosix Time.utc (Time.millisToPosix 0)
      , newTransaction =
            { category = ""
            , value = 0
            , date = ""
            , note = ""
            }
      , settings = { currencySymbol = "€", syncAddress = "" }
      }
    , Task.perform Msg.SetCurrentMonth Time.now
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Storage.modelUpdated UpdateFromStorage
        , Time.every (60 * 1000) NewTime
        ]
