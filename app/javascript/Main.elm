module Main exposing (..)

import Html exposing (Html, div, input, li, text, ul)
import Html.Events exposing (onInput)


-- MODEL


type alias Model =
    { todos : List Todo }


type alias Todo =
    { title : String
    }


testList : Model
testList =
    { todos = [ { title = "Thank Kyle for meeting with me." }, { title = "Send Paul Izra's contact information." } ] }


init : ( Model, Cmd Message )
init =
    ( testList, Cmd.none )



-- VIEW


view : Model -> Html Message
view model =
    div []
        [ ul [] <| List.map (\x -> li [] [ text x.title ]) model.todos
        , input [ onInput NewTodo ] []
        ]



-- MESSAGE


type Message
    = NewTodo String



-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NewTodo todoText ->
            let
                new =
                    { title = todoText }
            in
            ( { model | todos = new :: model.todos }, Cmd.none )



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
