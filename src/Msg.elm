module Msg exposing (..)

import Time


type Msg
    = NewTime Time.Posix
    | AddTransaction
    | AddTransactionNewValue String
    | AddTransactionNewCategory String
    | AddTransactionNewDate String
