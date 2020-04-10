module Update exposing (update)

import Msg exposing (..)
import Set
import Time


update msg model =
    case msg of
        NewTime time ->
            let
                monthOfYear =
                    { month = Time.toMonth Time.utc time, year = Time.toYear Time.utc time }
            in
            ( { model | currentMonth = monthOfYear }, Cmd.none )

        NewCategoryText text ->
            ( { model | newCategory = text }, Cmd.none )

        AddCategory ->
            ( { model
                | categories = Set.insert model.newCategory model.categories
                , newCategory = ""
              }
            , Cmd.none
            )
