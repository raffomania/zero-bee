port module StorageInterface exposing (connect, modelUpdated, storeModel, downloadBackup)

import Storage exposing (encodeModel)
import Json.Encode as E
import Model
import Util exposing (doubleCompare)


port sendModel : E.Value -> Cmd msg


port modelUpdated : (E.Value -> msg) -> Sub msg


port connect : String -> Cmd msg


port downloadBackup : () -> Cmd msg

storeModel : Model.Model -> Cmd msg
storeModel model =
    model
        |> encodeModel
        |> sendModel


