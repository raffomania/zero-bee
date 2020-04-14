module Msg exposing (..)

import Json.Decode
import Model
import Time


type Msg
    = NewTime Time.Posix
    | AddTransaction
    | AddTransactionNewValue String
    | AddTransactionNewCategory String
    | AddTransactionNewDate String
    | ChangeTransactionValue Model.Transaction String
    | ChangeBudgetEntry Model.MonthOfYear Model.CategoryId String
    | UpdateFromStorage Json.Decode.Value
    | NextMonth
    | PreviousMonth
