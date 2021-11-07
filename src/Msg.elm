module Msg exposing (..)

import Json.Decode
import Model
import Settings
import Time


type Msg
    = NewTime Time.Posix
    | SetCurrentMonth Time.Posix
    | AddTransaction
    | AddTransactionNewValue Model.Money
    | AddTransactionNewCategory String
    | AddTransactionNewDate String
    | AddTransactionNewNote String
    | AddTransactionNewAccount String
    | AddTransactionFocusCategoryInput Bool
    | ChangeTransactionValue Model.Transaction Model.Money
    | ChangeBudgetEntry Model.MonthOfYear Model.CategoryId Model.Money
    | UpdateFromStorage Json.Decode.Value
    | NextMonth
    | PreviousMonth
    | RemoveTransaction Model.Transaction
    | ChangePage Model.Page
    | NoOp
    | ChangeEditedBudgetEntry Model.MonthOfYear Model.CategoryId Model.Money
    | ChangeSettings Settings.Msg
