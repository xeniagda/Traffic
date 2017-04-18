import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task
import Json.Decode as Decode

decode : Decode.Decoder (String, Int)
decode =
    Decode.map2 (,)
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

main =
    Html.program {init=init, view=view, update=update, subscriptions=subscriptions}


type alias Model = {
    name : String,
    age : Int,
    err : Maybe String
}

init : (Model, Cmd Msg)
init = 
    (Model "" 0 Nothing, Task.perform identity <| Task.succeed Request)

type Msg
    = Request
    | SetText (Result Http.Error (String, Int))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetText (Ok (name, age)) ->
            ({model | name=name, age=age, err=Nothing}, Cmd.none)
        SetText (Err error) ->
            ({model | err = Just <| toString error}, Cmd.none)
        Request ->
            (model, Http.send SetText <| Http.get "http://localhost:8000/Hej" decode)

view : Model -> Html Msg
view model =
    div []
    [ p [] [text <| "Name: " ++ model.name]
    , p [] [text <| "Age: " ++ (toString model.age)]
    , p [style [("color", "red")]] [text <| "Error: " ++ toString model.err]
    , button [onClick Request] [text "Request!"]
    ]

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none