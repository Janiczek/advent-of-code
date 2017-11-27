module Advent exposing (program, Test)


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
                testResults =
                    tests
                        |> List.map (runTest parse compute)

                announce testResults =
                    Debug.log "Tests passed!" ()

                _ =
                    announce testResults
            in
                ( input |> parse |> compute |> Debug.log "Output"
                , Cmd.none
                )
        , update = (\_ model -> ( model, Cmd.none ))
        , subscriptions = (\_ -> Sub.none)
        }


type alias Test input output =
    { description : String
    , input : String
    , expectedParsedInput : input
    , expectedOutput : output
    }


runTest : (String -> input) -> (input -> output) -> Test input output -> ()
runTest parse compute { description, input, expectedParsedInput, expectedOutput } =
    let
        parsedInput =
            parse input

        output =
            compute parsedInput
    in
        if parsedInput /= expectedParsedInput then
            Debug.crash <| "\nTest \"" ++ description ++ "\" failed on `parse`:\n  input:    " ++ toString input ++ "\n  expected: " ++ toString expectedParsedInput ++ "\n  actual:   " ++ toString parsedInput ++ "\n"
        else if output /= expectedOutput then
            Debug.crash <| "\nTest \"" ++ description ++ "\" failed on `compute`:\n  input:    " ++ toString parsedInput ++ "\n  expected: " ++ toString expectedOutput ++ "\n  actual:   " ++ toString output ++ "\n"
        else
            ()
