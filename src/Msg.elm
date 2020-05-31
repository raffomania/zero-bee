module Msg exposing (..)

import Json.Decode
import Model
import Time


type Msg
    = NewTime Time.Posix
    | AddTransaction
    | AddTransactionNewValue Model.Money
    | AddTransactionNewCategory String
    | AddTransactionNewDate String
    | ChangeTransactionValue Model.Transaction Model.Money
    | ChangeBudgetEntry Model.MonthOfYear Model.CategoryId Model.Money
    | UpdateFromStorage Json.Decode.Value
    | NextMonth
    | PreviousMonth
    | RemoveTransaction Model.Transaction
    | RemoveBudgetEntry Model.MonthOfYear Model.CategoryId
    | ChangePage Model.Page
    | NoOp
    | ChangeCurrencySymbol String
    | ChangeEditedBudgetEntry Model.MonthOfYear Model.CategoryId Model.Money
