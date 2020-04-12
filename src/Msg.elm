module Msg exposing (..)

import Time
import Model


type Msg
    = NewTime Time.Posix
    | AddTransaction
    | AddTransactionNewValue String
    | AddTransactionNewCategory String
    | AddTransactionNewDate String
    | ChangeBudgetEntry Model.MonthOfYear Model.CategoryId String
