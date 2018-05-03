module Main exposing (..)

import Html exposing (Html, button, div, form, input, li, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onSubmit)
import Html.Keyed as Keyed


-- MODEL


type alias Model =
    { todos : List Todo
    , newTodo : String
    , uid : Int
    }


type alias Todo =
    { title : String
    , id : Int
    }


testList : Model
testList =
    { todos = [ { title = "Thank Kyle for meeting with me.", id = 1 }, { title = "Send Paul Izra's contact information.", id = 2 } ]
    , newTodo = ""
    , uid = 3
    }


init : ( Model, Cmd Message )
init =
    ( testList, Cmd.none )



-- VIEW


view : Model -> Html Message
view model =
    div []
        [ Keyed.ul [] <| List.map (\t -> viewKeyedTodo t) model.todos
        , Html.form [ onSubmit SubmitForm ]
            [ input [ onInput SetNewTodo, value model.newTodo ] []
            , button [] [ text "New Todo" ]
            ]
        ]


viewKeyedTodo : Todo -> ( String, Html msg )
viewKeyedTodo todo =
    ( toString todo.id, viewTodo todo )


viewTodo : Todo -> Html msg
viewTodo todo =
    li [] [ text todo.title ]



-- MESSAGE


type Message
    = SetNewTodo String
    | SubmitForm



-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        SetNewTodo todoText ->
            ( { model | newTodo = todoText }, Cmd.none )

        SubmitForm ->
            let
                todo =
                    { title = model.newTodo, id = model.uid }
            in
            ( { model | todos = todo :: model.todos, newTodo = "", uid = model.uid + 1 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Message
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
