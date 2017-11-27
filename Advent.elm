module Advent exposing (program, Test)


program :
    { input : String
    , parse1 : String -> input1
    , parse2 : String -> input2
    , compute1 : input1 -> output1
    , compute2 : input2 -> output2
    , tests1 : List (Test input1 output1)
    , tests2 : List (Test input2 output2)
    }
    -> Program Never ( output1, output2 ) Never
program { input, parse1, parse2, compute1, compute2, tests1, tests2 } =
    Platform.program
        { init =
            let
                testResults1 =
                    tests1
                        |> List.map (runTest "*" parse1 compute1)

                testResults2 =
                    tests2
                        |> List.map (runTest "**" parse2 compute2)

                announce testResults1 testResults2 =
                    Debug.log "Tests passed!" ()

                _ =
                    announce testResults1 testResults2
            in
                ( ( input
                        |> parse1
                        |> compute1
                        |> Debug.log "Output 1"
                  , input
                        |> parse2
                        |> compute2
                        |> Debug.log "Output 2"
                  )
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


runTest : String -> (String -> input) -> (input -> output) -> Test input output -> ()
runTest puzzleType parse compute { description, input, expectedParsedInput, expectedOutput } =
    let
        parsedInput =
            parse input

        output =
            compute parsedInput
    in
        if parsedInput /= expectedParsedInput then
            Debug.crash <| "\nTest \"" ++ description ++ "\" for " ++ puzzleType ++ " failed on `parse`:\n  input:    " ++ toString input ++ "\n  expected: " ++ toString expectedParsedInput ++ "\n  actual:   " ++ toString parsedInput ++ "\n"
        else if output /= expectedOutput then
            Debug.crash <| "\nTest \"" ++ description ++ "\" for " ++ puzzleType ++ " failed on `compute`:\n  input:    " ++ toString parsedInput ++ "\n  expected: " ++ toString expectedOutput ++ "\n  actual:   " ++ toString output ++ "\n"
        else
            ()
