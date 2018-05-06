module Main exposing (..)

import Html exposing (Html, button, div, form, h2, input, li, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onClick, onSubmit)
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
    , tag : TodoTag
    }


type TodoTag
    = Completed
    | Added
    | Removed
    | None


testList : Model
testList =
    { todos = [ { title = "Thank Kyle for meeting with me.", id = 1, tag = None }, { title = "Send Paul Izra's contact information.", id = 2, tag = None } ]
    , newTodo = ""
    , uid = 3
    }


init : ( Model, Cmd Message )
init =
    ( testList, Cmd.none )



-- VIEW


view : Model -> Html Message
view model =
    let
        todoFilter tag =
            List.filter (\t -> t.tag == tag) model.todos

        completed =
            todoFilter Completed

        added =
            todoFilter Added

        removed =
            todoFilter Removed

        none =
            todoFilter None
    in
    div []
        [ h2 [] [ text "Completed" ]
        , viewTodoList completed
        , h2 [] [ text "Added" ]
        , viewTodoList added
        , h2 [] [ text "Removed" ]
        , viewTodoList removed
        , h2 [] [ text "None" ]
        , viewTodoList none
        , Html.form [ onSubmit SubmitForm ]
            [ input [ onInput SetNewTodo, value model.newTodo ] []
            , button [] [ text "New Todo" ]
            ]
        ]


viewTodoList : List Todo -> Html Message
viewTodoList todos =
    Keyed.ul [] <| List.map (\t -> viewKeyedTodo t) todos


viewKeyedTodo : Todo -> ( String, Html Message )
viewKeyedTodo todo =
    ( toString todo.id, viewTodo todo )


viewTodo : Todo -> Html Message
viewTodo todo =
    li [onClick <| Toggle todo.id] [ text todo.title ]



-- MESSAGE


type Message
    = SetNewTodo String
    | SubmitForm
    | Toggle Int


-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        SetNewTodo todoText ->
            ( { model | newTodo = todoText }, Cmd.none )

        SubmitForm ->
            let
                todo =
                    { title = model.newTodo, id = model.uid, tag = Added }
            in
            ( { model | todos = todo :: model.todos, newTodo = "", uid = model.uid + 1 }, Cmd.none )

        Toggle todoId ->
            let
                toggleTodo t =
                    if t.id == todoId then
                        case t.tag of
                            Completed ->
                                {t | tag = None}
                            Added ->
                                {t | tag = Removed}
                            Removed ->
                                {t | tag = None}
                            None ->
                                {t | tag = Completed}
                    else
                        t
            in
                { model | todos = List.map toggleTodo model.todos} ! []
    


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
