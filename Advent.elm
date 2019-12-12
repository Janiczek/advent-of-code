module Advent exposing
    ( Test
    , pairings
    , program
    , removeNewlinesAtEnds
    , unsafeMaybe
    , unsafeResult
    , unsafeToInt
    )


program :
    { input : String
    , parse1 : String -> input1
    , parse2 : String -> input2
    , compute1 : input1 -> output1
    , compute2 : input2 -> output2
    , tests1 : List (Test input1 output1)
    , tests2 : List (Test input2 output2)
    }
    -> Program () ( output1, output2 ) Never
program { input, parse1, parse2, compute1, compute2, tests1, tests2 } =
    Platform.worker
        { init =
            \_ ->
                let
                    testResults1 =
                        tests1
                            |> List.reverse
                            |> List.map (runTest "*" parse1 compute1)

                    testResults2 =
                        tests2
                            |> List.reverse
                            |> List.map (runTest "**" parse2 compute2)

                    announce _ _ =
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
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


type alias Test input output =
    { description : String
    , input : String
    , expectedParsedInput : Maybe input
    , expectedOutput : output
    }


runTest : String -> (String -> input) -> (input -> output) -> Test input output -> ()
runTest puzzleType parse compute { description, input, expectedParsedInput, expectedOutput } =
    let
        _ =
            Debug.log
                ("Running test " ++ puzzleType)
                description
    in
    let
        parsedInput =
            parse input

        output =
            compute parsedInput
    in
    if
        (expectedParsedInput /= Nothing)
            && (expectedParsedInput /= Just parsedInput)
    then
        Debug.todo <|
            "\nTest \""
                ++ description
                ++ "\" for "
                ++ puzzleType
                ++ " failed on `parse`:\n  input:    "
                ++ Debug.toString input
                ++ "\n  expected: "
                ++ Debug.toString (unsafeMaybe "test - expected parsed input" expectedParsedInput)
                ++ "\n  actual:   "
                ++ Debug.toString parsedInput
                ++ "\n"

    else if output /= expectedOutput then
        Debug.todo <|
            "\nTest \""
                ++ description
                ++ "\" for "
                ++ puzzleType
                ++ " failed on `compute`:\n  input:    "
                ++ Debug.toString parsedInput
                ++ "\n  expected: "
                ++ Debug.toString expectedOutput
                ++ "\n  actual:   "
                ++ Debug.toString output
                ++ "\n"

    else
        ()


unsafeToInt : String -> Int
unsafeToInt string =
    string
        |> String.toInt
        |> Result.fromMaybe "Whatever"
        |> Result.mapError (\_ -> Debug.todo "Wrong input to unsafeToInt!")
        |> Result.withDefault 0


unsafeMaybe : String -> Maybe a -> a
unsafeMaybe location maybe =
    case maybe of
        Just x ->
            x

        Nothing ->
            Debug.todo location


unsafeResult : String -> Result err a -> a
unsafeResult location result =
    case result of
        Ok x ->
            x

        Err err ->
            Debug.todo ("[" ++ location ++ "]: " ++ Debug.toString err ++ "\n\n\n")


removeNewlinesAtEnds : String -> String
removeNewlinesAtEnds string =
    if String.startsWith "\n" string then
        removeNewlinesAtEnds (String.dropLeft 1 string)

    else if String.endsWith "\n" string then
        removeNewlinesAtEnds (String.dropRight 1 string)

    else
        string


pairings : List a -> List ( a, a )
pairings xs =
    case xs of
        [] ->
            []

        x :: xs_ ->
            List.map (\y -> ( x, y )) xs_ ++ pairings xs_
