module Advent exposing (program, Test1, Test2)


program :
    { input : String
    , parse : String -> input
    , compute1 : input -> output1
    , compute2 : input -> output2
    , tests1 : List (Test1 input output1)
    , tests2 : List (Test2 output2)
    }
    -> Program Never ( output1, output2 ) Never
program { input, parse, compute1, compute2, tests1, tests2 } =
    Platform.program
        { init =
            let
                testResults1 =
                    tests1
                        |> List.map (runTest1 parse compute1)

                testResults2 =
                    tests2
                        |> List.map (runTest2 parse compute2)

                announce testResults1 testResults2 =
                    Debug.log "Tests passed!" ()

                _ =
                    announce testResults1 testResults2

                parsedInput =
                    parse input
            in
                ( ( parsedInput |> compute1 |> Debug.log "Output 1"
                  , parsedInput |> compute2 |> Debug.log "Output 2"
                  )
                , Cmd.none
                )
        , update = (\_ model -> ( model, Cmd.none ))
        , subscriptions = (\_ -> Sub.none)
        }


type alias Test1 input output =
    { description : String
    , input : String
    , expectedParsedInput : input
    , expectedOutput : output
    }


type alias Test2 output =
    { description : String
    , input : String
    , expectedOutput : output
    }


runTest1 : (String -> input) -> (input -> output1) -> Test1 input output1 -> ()
runTest1 parse compute { description, input, expectedParsedInput, expectedOutput } =
    let
        parsedInput =
            parse input

        output =
            compute parsedInput
    in
        if parsedInput /= expectedParsedInput then
            Debug.crash <| "\nTest \"" ++ description ++ "\" failed on `parse`:\n  input:    " ++ toString input ++ "\n  expected: " ++ toString expectedParsedInput ++ "\n  actual:   " ++ toString parsedInput ++ "\n"
        else if output /= expectedOutput then
            Debug.crash <| "\nTest \"" ++ description ++ "\" failed on `compute1`:\n  input:    " ++ toString parsedInput ++ "\n  expected: " ++ toString expectedOutput ++ "\n  actual:   " ++ toString output ++ "\n"
        else
            ()


runTest2 : (String -> input) -> (input -> output2) -> Test2 output2 -> ()
runTest2 parse compute { description, input, expectedOutput } =
    let
        parsedInput =
            parse input

        output =
            input
                |> parse
                |> compute
    in
        if output /= expectedOutput then
            Debug.crash <| "\nTest \"" ++ description ++ "\" failed on `compute2`:\n  input:    " ++ toString parsedInput ++ "\n  expected: " ++ toString expectedOutput ++ "\n  actual:   " ++ toString output ++ "\n"
        else
            ()
