module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input, br)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing(type_, value)


type alias Model =
    { str : String
    , list : List String
    , listForView : List String
    }


initialModel : Model
initialModel =
    { str = ""
    , list = []
    , listForView = []
    }


type Msg
    = Hold String
    | Pressed Operation
    |Update (List String)

type Operation
    = Add
    | Search


update : Msg -> Model -> Model
update msg model =
    case msg of
        Hold str ->
            { model | str=str }
        Pressed operation ->
          case operation of
            Add ->
              { model |
                list = model.str :: model.list
              , listForView = model.str :: model.listForView
              , str = ""
              }
            Search ->
              model
                |> search model.str
        Update list ->
            { model | list = list }

deleteFromList str model =
  let
    filter =
        List.filter ((/=)str)
  in
    { model |
      list = filter model.list
    , listForView = filter model.listForView }
    
search str model =
  let
    searchStr =
        List.filter (String.contains str)
  in
    { model | listForView = searchStr model.listForView }



view : Model -> Html Msg
view model =
  let
    viewList =
      model.list
        |> List.indexedMap (\i x ->
          let
            updatedList =
              model.list
                |> List.indexedMap (\j y -> ( j, y ))
                |> List.foldr (\( j, y ) li -> if i==j then li else y::li) [] 
          in
            ( x, updatedList )
        )
        |> List.concatMap (\( str, updatedList ) ->
          [ str |> text
          , button[onClick <| Update updatedList][text "×"]
          , br[][]
          ]
        )
  in
    div []
        <| List.append
            [ input [ type_ "textbox"
                , value model.str
                , onInput Hold
                ][]
            , button[onClick <| Pressed Add][text "add"]
            , button[onClick <| Pressed Search][text "search"]
            , br[][]
            ]
            viewList


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
