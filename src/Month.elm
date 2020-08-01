module Month exposing (increment, decrement)

import Date
import Model exposing (MonthOfYear)
import Time


decrement : MonthOfYear -> MonthOfYear
decrement month =
    case month.month of
        Time.Jan ->
            { month = Time.Dec, year = month.year - 1 }

        _ ->
            { month | month = Date.numberToMonth <| Date.monthToNumber month.month - 1 }


increment : MonthOfYear -> MonthOfYear
increment month =
    case month.month of
        Time.Dec ->
            { month = Time.Jan, year = month.year + 1 }

        _ ->
            { month | month = Date.numberToMonth <| Date.monthToNumber month.month + 1 }
