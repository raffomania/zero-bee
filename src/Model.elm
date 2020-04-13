module Model exposing (..)

import Date
import Dict exposing (Dict)
import Set exposing (Set)
import Time


type alias Model =
    { transactions : List Transaction
    , currentMonth : MonthOfYear
    , budgetEntries : Dict MonthIndex (Dict CategoryId BudgetEntry)
    , newTransaction :
        { value : String
        , date : String
        , category : CategoryId
        }
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


getBudgetEntry : CategoryId -> MonthOfYear -> Model -> Maybe BudgetEntry
getBudgetEntry category month model =
    model.budgetEntries
        |> Dict.get (getMonthIndex month)
        |> Maybe.andThen (Dict.get category)


dateToMonth : Date.Date -> MonthOfYear
dateToMonth date =
    { month = Date.month date
    , year = Date.year date
    }


isTransactionInMonth month transaction =
    month == dateToMonth transaction.date
