port module Year2016.Day05 exposing (..)

import Json.Decode


port elmAsks : String -> Cmd msg


port jsAnswers : (String -> msg) -> Sub msg


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { id : String
    , counter : Int
    , password : String
    }


type Msg
    = JsAnswers String


init : ( Model, Cmd Msg )
init =
    ( initModel, elmAsks (makeString initModel.id initModel.counter) )


initModel : Model
initModel =
    -- abc: 18f47a30
    { id = "wtnhxymk"
    , counter = 0
    , password = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JsAnswers hash ->
            if isGood hash then
                let
                    passwordChar =
                        hash
                            |> String.dropLeft 5
                            |> String.left 1

                    newPassword =
                        model.password ++ passwordChar

                    enough =
                        haveEnough newPassword

                    newCounter =
                        (model.counter + 1)
                            |> Debug.log ("new counter after password char " ++ passwordChar)

                    newModel =
                        { model
                            | password = newPassword
                            , counter = newCounter
                        }
                in
                    ( if enough then
                        let
                            _ =
                                Debug.log "password!" newPassword
                        in
                            newModel
                      else
                        newModel
                    , if not enough then
                        elmAsks (makeString model.id newCounter)
                      else
                        Cmd.none
                    )
            else
                let
                    newCounter =
                        model.counter + 1
                in
                    ( { model | counter = newCounter }
                    , elmAsks (makeString model.id newCounter)
                    )


haveEnough : String -> Bool
haveEnough password =
    String.length password == 8


subscriptions : Model -> Sub Msg
subscriptions model =
    jsAnswers JsAnswers


makeString : String -> Int -> String
makeString id counter =
    id ++ toString counter


isGood : String -> Bool
isGood hash =
    String.left 5 hash == "00000"
