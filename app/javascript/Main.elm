module Main exposing (..)

import Html exposing (Html, button, div, form, h2, input, li, text, ul)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput, onSubmit)
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
    { todos =
        [ { title = "Thank Kyle for meeting with me.", id = 1, tag = None }
        , { title = "Send Paul Izra's contact information.", id = 2, tag = None }
        , { title = "Figure out how to switch types easily", id = 3, tag = None }
        , { title = "Should this be one big text box or more graphically structured?", id = 4, tag = Added }
        , { title = "I definitely need no latency in typing.", id = 4, tag = Added }
        , { title = "What happens when I want to switch the order?", id = 5, tag = Completed }
        , { title = "Watch TV", id = 6, tag = Completed }
        , { title = "Ben's graduation present", id = 7, tag = Removed }
        , { title = "Get $40 from the ATM", id = 8, tag = Removed }
        , { title = "How can we navigate between todos that get out of order?", id = 9, tag = None }
        , { title = "Filter todos by date added", id = 10, tag = None }
        ]
    , newTodo = ""
    , uid = 11
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
        [ h2 [] [ text "Jake's plan for May 6th, 2018" ]
        , viewTodoList completed
        , viewTodoList added
        , viewTodoList removed
        , viewTodoList none
        , Html.form [ onSubmit SubmitForm ]
            [ input [ onInput SetNewTodo, value model.newTodo ] []
            , button [] [ text "New Todo" ]
            ]
        ]


viewTodoList : List Todo -> Html Message
viewTodoList todos =
    Keyed.ul [ class "todo-list" ] <| List.map (\t -> viewKeyedTodo t) todos


viewKeyedTodo : Todo -> ( String, Html Message )
viewKeyedTodo todo =
    ( toString todo.id, viewTodo todo )


viewTodo : Todo -> Html Message
viewTodo todo =
    li [ onClick <| Toggle todo.id ] [ text <| viewTag todo, text todo.title ]


viewTag : Todo -> String
viewTag todo =
    case todo.tag of
        Completed ->
            "C "

        Added ->
            "+ "

        Removed ->
            "- "

        None ->
            "  "



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
                                { t | tag = Added }

                            Added ->
                                { t | tag = Removed }

                            Removed ->
                                { t | tag = None }

                            None ->
                                { t | tag = Completed }
                    else
                        t
            in
            ( { model | todos = List.map toggleTodo model.todos |> putTodoAtFront todoId }, Cmd.none )


putTodoAtFront : Int -> List Todo -> List Todo
putTodoAtFront todoId todos =
    let
        ( todo, rest ) =
            List.partition (\t -> t.id == todoId) todos
    in
    List.append todo rest



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
