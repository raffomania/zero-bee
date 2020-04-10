module Model exposing (Model, Transaction)

import Dict exposing (Dict)
import Set exposing (Set)
import Time


type alias Model =
    { transactions : List Transaction
    , categories : Set CategoryId
    , currentMonth : Maybe Time.Month
    , budgetEntries : Dict Time.Month (Dict CategoryId BudgetEntry)
    , newCategory : String
    }


type alias CategoryId =
    String


type alias Money =
    Int


type alias Transaction =
    { value : Money
    , time : Time.Posix
    , category : CategoryId
    }


type alias BudgetEntry =
    { month : Time.Month,
          year : Int
    , value : Money
    , category : CategoryId
    }
