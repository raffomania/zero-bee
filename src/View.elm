module View exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Model exposing (Model)
import Msg exposing (..)
import Set


view : Model -> Browser.Document Msg
view model =
    { title = "whyNAB"
    , body = [ newCategoryForm model, br [] [], budgetView model ]
    }


newCategoryForm : Model -> Html Msg
newCategoryForm model =
    Html.form [ onSubmit AddCategory ]
        [ input [ type_ "text", placeholder "new category", value model.newCategory, onInput NewCategoryText ] []
        ]


budgetView : Model -> Html Msg
budgetView model =
    table []
        (tr []
            [ th [] [ text "category" ]
            , th [] [ text "budgeted" ]
            , th [] [ text "activity" ]
            , th [] [ text "available" ]
            ]
            :: (model.categories
                    |> Set.toList
                    |> List.map budgetEntry
               )
        )


budgetEntry name =
    tr [] [ td [] [ text name ] ]
