module Main exposing (init, main)

import Browser
import Date
import Dict
import Model exposing (Model)
import Msg exposing (Msg)
import StorageInterface
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
      , editingTransaction = Nothing
      , currentPage = Model.Budget
      , date = Date.fromPosix Time.utc (Time.millisToPosix 0)
      , newTransaction =
            { category = ""
            , value = 0
            , dayOfMonth = "1"
            , note = ""
            , account = ""
            , focus = Nothing
            }
      , settings = { currencySymbol = "€", syncAddress = "" }
      }
    , Cmd.batch
        [ Task.perform Msg.SetCurrentMonth Time.now
        , Task.perform Msg.NewTime Time.now
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ StorageInterface.modelUpdated Msg.UpdateFromStorage
        , Time.every (60 * 1000) Msg.NewTime
        ]
