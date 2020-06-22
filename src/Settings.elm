module Settings exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Msg exposing (..)
import Util


view model =
    column []
        [ Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "Currency Symbol"
            , text = model.settings.currencySymbol
            , onChange = ChangeCurrencySymbol
            }
        , Input.text [ Util.onEnter ConnectRemoteStorage ]
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "RemoteStorage sync address"
            , text = model.settings.syncAddress
            , onChange = ChangeSyncAddress
            }
        ]
