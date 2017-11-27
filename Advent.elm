module Advent exposing (program, Test(..))


program :
    { input : String
    , parse : String -> input
    , compute : input -> output
    , tests : List (Test input output)
    }
    -> Program Never output Never
program { input, parse, compute, tests } =
    Platform.program
        { init =
            let
                results =
                    tests
                        |> List.map (runTest parse compute)
            in
                ( input |> parse |> compute |> Debug.log "Output"
                , Cmd.none
                )
        , update = (\_ model -> ( model, Cmd.none ))
        , subscriptions = (\_ -> Sub.none)
        }


type Test input output
    = ParseTest String String input
    | ComputeTest String input output


runTest : (String -> input) -> (input -> output) -> Test input output -> ()
runTest parse compute test =
    case test of
        ParseTest description input expectedOutput ->
            let
                output =
                    parse input
            in
                if output /= expectedOutput then
                    Debug.crash <| "\nParse test \"" ++ description ++ "\" failed:\n  input:    " ++ toString input ++ "\n  expected: " ++ toString expectedOutput ++ "\n  actual:   " ++ toString output ++ "\n"
                else
                    ()

        ComputeTest description input expectedOutput ->
            let
                output =
                    compute input
            in
                if output /= expectedOutput then
                    Debug.crash <| "\nCompute test \"" ++ description ++ "\" failed:\n  input:    " ++ toString input ++ "\n  expected: " ++ toString expectedOutput ++ "\n  actual:   " ++ toString output ++ "\n"
                else
                    ()
