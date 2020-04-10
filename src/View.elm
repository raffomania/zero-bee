module View exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Model exposing (..)
import Msg exposing (..)
import Set


view : Model -> Browser.Document Msg
view model =
    { title = "whyNAB"
    , body = [ newCategoryForm model, br [] [], monthPicker model, budgetView model ]
    }


monthPicker : Model -> Html Msg
monthPicker model =
    let
        label =
            Debug.toString model.currentMonth.month ++ " " ++ String.fromInt model.currentMonth.year
    in
    text label


newCategoryForm : Model -> Html Msg
newCategoryForm model =
    Html.form [ onSubmit AddCategory ]
        [ input [ type_ "text", placeholder "new category", value model.newCategory, onInput NewCategoryText ] []
        ]


budgetView : Model -> Html Msg
budgetView model =
    table [ attribute "border" "1" ]
        (tr []
            [ th [] [ text "category" ]
            , th [] [ text "budgeted" ]
            , th [] [ text "activity" ]
            , th [] [ text "available" ]
            ]
            :: (model.categories
                    |> Set.toList
                    |> List.map (budgetEntry model)
               )
        )

budgetEntry : Model -> CategoryId -> Html Msg
budgetEntry model name =
    let entry =
            Model.getBudgetEntry name model
                |> Maybe.withDefault {
                       month = model.currentMonth,
                       value = 0,
                       category = name
                   }
    in
    tr [] [
         td [] [ text name ],
         td [] [ input [ value <| String.fromInt entry.value ] []]
        ]
