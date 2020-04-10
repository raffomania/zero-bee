module Msg exposing (..)

import Time


type Msg
    = NewTime Time.Posix
    | NewCategoryText String
    | AddCategory
    | AddTransaction
    | AddTransactionNewValue String
    | AddTransactionNewCategory String
    | AddTransactionNewDate String
