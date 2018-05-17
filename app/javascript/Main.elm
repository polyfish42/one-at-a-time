module Main exposing (..)

import Html exposing (Html, button, div, form, h2, input, li, text, ul)
import Html.Attributes exposing (checked, class, type_, value)
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
    , addedToday : Bool
    }


type TodoTag
    = Completed
    | Removed
    | None


testList : Model
testList =
    { todos =
        [ { title = "Thank Kyle for meeting with me.", id = 1, tag = None, addedToday = True }
        , { title = "Send Paul Izra's contact information.", id = 2, tag = None, addedToday = False }
        , { title = "Figure out how to switch types easily", id = 3, tag = None, addedToday = False }
        , { title = "Should this be one big text box or more graphically structured?", id = 4, tag = None, addedToday = True }
        , { title = "What happens when I want to switch the order?", id = 5, tag = Completed, addedToday = False }
        , { title = "Watch TV", id = 6, tag = Completed, addedToday = False }
        , { title = "Ben's graduation present", id = 7, tag = Removed, addedToday = False }
        , { title = "Get $40 from the ATM", id = 8, tag = Removed, addedToday = False }
        , { title = "How can we navigate between todos that get out of order?", id = 9, tag = None, addedToday = False }
        , { title = "Filter todos by date added", id = 10, tag = None, addedToday = False }
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

        none =
            todoFilter None

        removed =
            todoFilter Removed
    in
    div []
        [ h2 [] [ text "Jake's plan for May 6th, 2018" ]
        , viewTodoList completed
        , Html.form [ onSubmit SubmitForm ]
            [ input [ onInput SetNewTodo, value model.newTodo ] []
            , button [] [ text "New Todo" ]
            ]
        , viewTodoList none
        , viewTodoList removed
        ]


viewTodoList : List Todo -> Html Message
viewTodoList todos =
    Keyed.ul [ class "todo-list" ] <| List.map (\t -> viewKeyedTodo t) todos


viewKeyedTodo : Todo -> ( String, Html Message )
viewKeyedTodo todo =
    ( toString todo.id, viewTodo todo )


viewTodo : Todo -> Html Message
viewTodo todo =
    li [] [ text <| todoAddedTodayPlus todo, input [ type_ "checkbox", todoChecked todo, onClick <| ToggleComplete todo.id ] [], text todo.title ]


todoAddedTodayPlus : Todo -> String
todoAddedTodayPlus todo =
    if todo.addedToday && todo.tag /= Completed then
        "+"
    else
        " "

todoChecked : Todo -> Html.Attribute msg
todoChecked todo =
    case todo.tag of
        Completed ->
            checked True

        _ ->
            checked False



-- MESSAGE


type Message
    = SetNewTodo String
    | SubmitForm
    | ToggleComplete Int



-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        SetNewTodo todoText ->
            ( { model | newTodo = todoText }, Cmd.none )

        SubmitForm ->
            let
                todo =
                    { title = model.newTodo, id = model.uid, tag = None, addedToday = True }
            in
            ( { model | todos = todo :: model.todos, newTodo = "", uid = model.uid + 1 }, Cmd.none )

        ToggleComplete todoId ->
            let
                completeTodo todo =
                    if todo.id == todoId then
                        case todo.tag of
                            Completed ->
                                { todo | tag = None }

                            _ ->
                                { todo | tag = Completed }
                    else
                        todo
            in
            ( { model | todos = List.map completeTodo model.todos }, Cmd.none )


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
