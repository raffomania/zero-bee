port module StorageInterface exposing (connect, downloadBackup, modelUpdated, storeModel)

import Json.Encode as E
import Model
import Storage exposing (encodeModel)


port sendModel : E.Value -> Cmd msg


port modelUpdated : (E.Value -> msg) -> Sub msg


port connect : String -> Cmd msg


port downloadBackup : () -> Cmd msg


storeModel : Model.Model -> Cmd msg
storeModel model =
    model
        |> encodeModel
        |> sendModel
