port module Year201X.DayXX exposing (..)

import Json.Decode


port elmAsks : String -> Cmd msg


port jsAnswers : (String -> msg) -> Sub msg


main : Program Never Model Msg
main =
    Platform.program
        { init = init1
        , update = update1

        -- { init = init2
        -- , update = update2
        , subscriptions = subscriptions
        }


type alias Model =
    Int


type Msg
    = JsAnswers String


init1 : ( Model, Cmd Msg )
init1 =
    ( initModel1, elmAsks (makeString initModel.id initModel.counter) )


initModel1 : Model
initModel1 =
    0


init2 : ( Model, Cmd Msg )
init2 =
    ( initModel2, elmAsks (makeString initModel.id initModel.counter) )


initModel2 : Model
initModel2 =
    0


update1 : Msg -> Model -> ( Model, Cmd Msg )
update1 msg model =
    case msg of
        JsAnswers hash ->
            ( model, Cmd.none )


toInt : String -> Int
toInt string =
    string
        |> String.toInt
        |> Result.mapError (\_ -> Debug.crash "Wrong input!")
        |> Result.withDefault 0


update2 : Msg -> Model -> ( Model, Cmd Msg )
update2 msg model =
    case msg of
        JsAnswers hash ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    jsAnswers JsAnswers


prepareForPort : String -> String
prepareForPort string =
    string
