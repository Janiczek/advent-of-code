module Advent exposing (program)


program :
    { input : input
    , init : input -> output
    }
    -> Program Never output Never
program { input, init } =
    Platform.program
        { init =
            ( init input |> Debug.log "Output"
            , Cmd.none
            )
        , update = (\_ model -> ( model, Cmd.none ))
        , subscriptions = (\_ -> Sub.none)
        }
