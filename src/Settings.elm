module Settings exposing (view, update, Msg)

import Element exposing (column, Element, text, spacing, padding)
import Element.Input as Input
import Util
import StorageInterface
import Model exposing (Model)


type Msg = ChangeCurrencySymbol String | ChangeSyncAddress String | ConnectRemoteStorage | DownloadBackup

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeSyncAddress newAddress ->
            let
                settings =
                    model.settings

                updatedSettings = { settings | syncAddress = newAddress}

            in
            ( { model | settings = updatedSettings }, Cmd.none )

        ConnectRemoteStorage ->
            ( model, Cmd.batch [ StorageInterface.connect model.settings.syncAddress, StorageInterface.storeModel model ] )

        ChangeCurrencySymbol symbol ->
            let
                settings =
                    model.settings

                updatedSettings =
                    { settings | currencySymbol = symbol }

                updatedModel =
                    { model | settings = updatedSettings }
            in
            ( updatedModel, StorageInterface.storeModel updatedModel )
        DownloadBackup ->
            (model, StorageInterface.downloadBackup ())



view : Model.Settings -> Element Msg
view model =
    column [spacing 20]
        [ Input.text []
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "Currency Symbol"
            , text = model.currencySymbol
            , onChange = ChangeCurrencySymbol
            }
        , Input.text [ Util.onEnter ConnectRemoteStorage ]
            { placeholder = Nothing
            , label = Input.labelAbove [] <| text "RemoteStorage sync address"
            , text = model.syncAddress
            , onChange = ChangeSyncAddress
            }

        , Input.button [Util.class "bh", padding 10] {
            onPress = Just DownloadBackup,
            label = text "Download a backup of your data"
        }
        ]

