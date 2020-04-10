module View exposing (view)

import Browser
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Model exposing (..)
import Msg exposing (..)
import Set


view : Model -> Browser.Document Msg
view model =
    { title = "whyNAB"
    , body =
        [ newCategoryForm model
        , addTransactionForm model
        , transactionList model
        , br [] []
        , monthPicker model
        , budgetView model
        ]
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


addTransactionForm : Model -> Html Msg
addTransactionForm model =
    Html.form [ onSubmit AddTransaction ]
        [ text "new transaction"
        , br [] []
        , input [ type_ "text", value <| String.fromInt <| model.newTransaction.value, onInput AddTransactionNewValue ] []
        , categorySelect model.categories model.newTransaction.category
        , input [ type_ "text", onInput AddTransactionNewDate ] []
        , text ("date: " ++ (Debug.toString <| Maybe.map Date.toIsoString model.newTransaction.date))
        , button [ type_ "submit" ] [ text "submit" ]
        ]


categorySelect categories selected =
    select [ placeholder "category", value <| selected, onInput AddTransactionNewCategory ]
        (Set.toList categories
            |> List.map (text >> List.singleton >> option [])
        )


transactionList model =
    table []
        (tr [] [
             th [] [text "date"],
             th [] [text "category"],
             th [] [text "value"]
            ]
        ::
        (model.transactions
            |> List.map
                (\t ->
                    tr []
                        [ td [] [ text <| Date.toIsoString t.date ]
                        , td [] [ text <| t.category ]
                        , td [] [ text <| String.fromInt t.value ]
                        ]
                )
        ))


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
    let
        entry =
            Model.getBudgetEntry name model
                |> Maybe.withDefault
                    { month = model.currentMonth
                    , value = 0
                    , category = name
                    }
    in
    tr []
        [ td [] [ text name ]
        , td [] [ input [ value <| String.fromInt entry.value ] [] ]
        , td [] [ text "0" ]
        , td [] [ text (String.fromInt entry.value) ]
        ]
