port module Year2016.Day05 exposing (..)

import Json.Decode


port elmAsks : String -> Cmd msg


port jsAnswers : (String -> msg) -> Sub msg


main : Program Never Model Msg
main =
    Platform.program
        -- { init = init1
        -- , update = update1
        { init = init2
        , update = update2
        , subscriptions = subscriptions
        }


type alias Model =
    { id : String
    , counter : Int
    , password : String
    }


type Msg
    = JsAnswers String


init1 : ( Model, Cmd Msg )
init1 =
    ( initModel1, elmAsks (makeString initModel.id initModel.counter) )


initModel1 : Model
initModel1 =
    -- abc: 18f47a30
    { id = "wtnhxymk"
    , counter = 0
    , password = ""
    }


init2 : ( Model, Cmd Msg )
init2 =
    ( initModel2, elmAsks (makeString initModel.id initModel.counter) )


initModel2 : Model
initModel2 =
    -- abc: 05ace8e3
    { id = "wtnhxymk"
    , counter = 0
    , password = "________"
    }


update1 : Msg -> Model -> ( Model, Cmd Msg )
update1 msg model =
    case msg of
        JsAnswers hash ->
            if isGood1 hash then
                let
                    passwordChar =
                        hash
                            |> String.dropLeft 5
                            |> String.left 1

                    newPassword =
                        model.password ++ passwordChar

                    enough =
                        haveEnough1 newPassword

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
            if isGood2 hash then
                let
                    position =
                        hash
                            |> String.dropLeft 5
                            |> String.left 1
                            |> toInt

                    passwordChar =
                        hash
                            |> String.dropLeft 6
                            |> String.left 1

                    newPassword =
                        updatePassword model.password position passwordChar

                    enough =
                        haveEnough2 newPassword

                    newCounter =
                        (model.counter + 1)
                            |> Debug.log ("new counter with password " ++ newPassword)

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


updatePassword : String -> Int -> String -> String
updatePassword password position letter =
    password
        |> String.toList
        |> List.map String.fromChar
        |> List.indexedMap
            (\i char ->
                if i == position && char == "_" then
                    letter
                else
                    char
            )
        |> String.join ""


haveEnough1 : String -> Bool
haveEnough1 password =
    String.length password == 8


haveEnough2 : String -> Bool
haveEnough2 password =
    password
        |> String.contains "_"
        |> not


subscriptions : Model -> Sub Msg
subscriptions model =
    jsAnswers JsAnswers


makeString : String -> Int -> String
makeString id counter =
    id ++ toString counter


isGood1 : String -> Bool
isGood1 hash =
    String.left 5 hash == "00000"


isGood2 : String -> Bool
isGood2 hash =
    (String.left 5 hash == "00000")
        && (hash
                |> String.dropLeft 5
                |> String.left 1
                |> (flip List.member) [ "0", "1", "2", "3", "4", "5", "6", "7" ]
           )
