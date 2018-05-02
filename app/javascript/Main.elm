module Main exposing (..)

import Html exposing (Html, text, li,  ul)


-- MODEL


type alias Model =
    { todos : List Todo }


type alias Todo =
    { title : String
    }


testList : Model
testList =
    { todos = [ { title = "Thank Kyle for meeting with me." }, { title = "Send Paul Izra's contact information."} ] }


init : ( Model, Cmd Message )
init =
    ( testList, Cmd.none )



-- VIEW


view : Model -> Html Message
view model =
    ul [] <|  List.map (\x -> li [] [text x.title]) model.todos



-- MESSAGE


type Message
    = None



-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    ( model, Cmd.none )



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
