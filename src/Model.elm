module Model exposing (..)

import Date
import Dict exposing (Dict)
import Set exposing (Set)
import Time


type alias Model =
    { transactions : List Transaction
    , categories : Set CategoryId
    , currentMonth : MonthOfYear
    , budgetEntries : Dict MonthIndex (Dict CategoryId BudgetEntry)
    , newCategory : String
    , newTransaction : Transaction
    }


type alias CategoryId =
    String


type alias Money =
    Int


type alias MonthIndex =
    String


type alias MonthOfYear =
    { month : Time.Month
    , year : Int
    }


type alias Transaction =
    { value : Money
    , date : Date.Date
    , category : CategoryId
    }


type alias BudgetEntry =
    { month : MonthOfYear
    , value : Money
    , category : CategoryId
    }


getMonthIndex : MonthOfYear -> MonthIndex
getMonthIndex monthYear =
    String.concat
        [ String.fromInt <| monthYear.year
        , "-"
        , String.fromInt <| Date.monthToNumber <| monthYear.month
        ]


getBudgetEntry : CategoryId -> Model -> Maybe BudgetEntry
getBudgetEntry category model =
    model.budgetEntries
        |> Dict.get (getMonthIndex model.currentMonth)
        |> Maybe.andThen (Dict.get category)
