module Settings exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Msg exposing (..)


view model =
    Input.text []
        { placeholder = Nothing
        , label = Input.labelAbove [] <| text "Currency Symbol"
        , text = model.settings.currencySymbol
        , onChange = ChangeCurrencySymbol
        }
