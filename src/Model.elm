module Model exposing (..)

import Date
import Dict exposing (Dict)
import Time


type alias Model =
    { transactions : List Transaction
    , currentMonth : MonthOfYear
    , budgetEntries : Dict MonthIndex (Dict CategoryId BudgetEntry)
    , editingBudgetEntry : Maybe BudgetEntryEdit
    , currentPage : Page
    , date : Date.Date
    , newTransaction :
        { value : Money
        , dayOfMonth : String
        , category : CategoryId
        , note : String
        , account : String
        }
    , settings : Settings
    }

type alias Settings =
    { currencySymbol : String
    , syncAddress : String
    }


type Page
    = Transactions
    | Budget
    | SettingsPage


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
    , note : String
    , account: String
    }


type alias BudgetEntry =
    { value : Money
    , category : CategoryId
    }


type alias BudgetEntryEdit =
    { value : Money
    , category : CategoryId
    , month : MonthOfYear
    }

getMonthIndex : MonthOfYear -> MonthIndex
getMonthIndex monthYear =
    String.concat
        [ String.fromInt <| monthYear.year
        , "-"
        , String.fromInt <| Date.monthToNumber <| monthYear.month
        ]


parseMonthIndex : String -> MonthOfYear
parseMonthIndex index =
    case
        String.split "-" index
    of
        [ year, month ] ->
            { year = String.toInt year |> Maybe.withDefault 2020
            , month = Date.numberToMonth (String.toInt month |> Maybe.withDefault 1)
            }

        _ ->
            { year = 2020
            , month = Time.Jan
            }


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


monthToDate : MonthOfYear -> Date.Date
monthToDate month =
    Date.fromCalendarDate month.year month.month 1


isTransactionInMonth : MonthOfYear -> Transaction -> Bool
isTransactionInMonth month transaction =
    month == dateToMonth transaction.date


isInFuture : MonthOfYear -> Transaction -> Bool
isInFuture currentMonth transaction =
    compareMonths currentMonth (dateToMonth transaction.date) == LT


compareMonths : MonthOfYear -> MonthOfYear -> Order
compareMonths a b =
    if a == b then
        EQ

    else if a.year > b.year then
        GT

    else if a.year < b.year then
        LT

    else if Date.monthToNumber a.month > Date.monthToNumber b.month then
        GT

    else
        LT
