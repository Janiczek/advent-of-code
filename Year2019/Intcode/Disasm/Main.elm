module Year2019.Intcode.Disasm.Main exposing (main)

import Array
import Browser
import Html exposing (Attribute, Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Year2019.Intcode as Intcode exposing (Mask(..), Memory, Parameter(..))
import Year2019.Intcode.Disasm as Disasm exposing (Data(..))


type IntcodeProgram
    = Raw String
    | Parsed ( String, Memory )
    | ParsedAndDisassembled ( String, Memory, List ( Int, Data Op ) )


type alias Model =
    { program : IntcodeProgram
    , ops : List ( Int, Intcode.Op Op )
    }


type Msg
    = SetRawProgram String


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { program = Raw ""
      , ops = supportedOps -- TODO allow the user to change this
      }
    , Cmd.none
    )


supportedOps : List ( Int, Intcode.Op Op )
supportedOps =
    [ ( 1
      , Intcode.Op3
            ( DontCare, DontCare, WantPosition )
            (\addr0 addr1 dest ->
                Add
                    { addr0 = addr0
                    , addr1 = addr1
                    , dest = dest
                    }
            )
      )
    , ( 2
      , Intcode.Op3
            ( DontCare, DontCare, WantPosition )
            (\addr0 addr1 dest ->
                Mult
                    { addr0 = addr0
                    , addr1 = addr1
                    , dest = dest
                    }
            )
      )
    , ( 3
      , Intcode.Op1 WantPosition (\dest -> SetInputAt { dest = dest })
      )
    , ( 4, Intcode.Op1 DontCare (\addr -> Print { addr = addr }) )
    , ( 5
      , Intcode.Op2 ( DontCare, DontCare )
            (\test jumpTo ->
                JumpIfTrue
                    { test = test
                    , jumpTo = jumpTo
                    }
            )
      )
    , ( 6
      , Intcode.Op2 ( DontCare, DontCare )
            (\test jumpTo ->
                JumpIfFalse
                    { test = test
                    , jumpTo = jumpTo
                    }
            )
      )
    , ( 7
      , Intcode.Op3 ( DontCare, DontCare, WantPosition )
            (\left right dest ->
                LessThan
                    { left = left
                    , right = right
                    , dest = dest
                    }
            )
      )
    , ( 8
      , Intcode.Op3 ( DontCare, DontCare, WantPosition )
            (\left right dest ->
                Equals
                    { left = left
                    , right = right
                    , dest = dest
                    }
            )
      )
    , ( 99, Intcode.Op0 Halt )
    ]


type Op
    = Add { addr0 : Parameter, addr1 : Parameter, dest : Parameter }
    | Mult { addr0 : Parameter, addr1 : Parameter, dest : Parameter }
    | SetInputAt { dest : Parameter }
    | Print { addr : Parameter }
    | JumpIfTrue { test : Parameter, jumpTo : Parameter }
    | JumpIfFalse { test : Parameter, jumpTo : Parameter }
    | LessThan { left : Parameter, right : Parameter, dest : Parameter }
    | Equals { left : Parameter, right : Parameter, dest : Parameter }
    | Halt


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        SetRawProgram string ->
            { model
                | program =
                    string
                        |> String.replace "\n" ""
                        |> String.replace "\t" ""
                        |> String.replace " " ""
                        |> Raw
            }
                |> recalc
    , Cmd.none
    )


recalc : Model -> Model
recalc model =
    let
        rawProgram =
            getRawProgram model.program

        maybeMem : Maybe Memory
        maybeMem =
            Intcode.parseSafe rawProgram

        maybeDisassembled : Maybe (List ( Int, Data Op ))
        maybeDisassembled =
            Maybe.map
                (Disasm.disassembleWith model.ops)
                maybeMem
    in
    case ( maybeMem, maybeDisassembled ) of
        ( Nothing, _ ) ->
            model

        ( Just mem, Nothing ) ->
            { model
                | program =
                    Parsed
                        ( rawProgram
                        , mem
                        )
            }

        ( Just mem, Just disassembled ) ->
            { model
                | program =
                    ParsedAndDisassembled
                        ( rawProgram
                        , mem
                        , disassembled
                        )
            }


getRawProgram : IntcodeProgram -> String
getRawProgram program =
    case program of
        Raw p ->
            p

        Parsed ( p, _ ) ->
            p

        ParsedAndDisassembled ( p, _, _ ) ->
            p


twoColsStyle : List (Attribute Msg)
twoColsStyle =
    [ Attrs.style "display" "flex"
    , Attrs.style "flex-direction" "row"
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Intcode Disassembler"
    , body =
        case model.program of
            Raw string ->
                [ viewString string
                , Html.div twoColsStyle
                    [ Html.div [] [ Html.text "Memory unavailable" ]
                    , Html.div [] [ Html.text "Disassembly unavailable" ]
                    ]
                ]

            Parsed ( string, mem ) ->
                [ viewString string
                , Html.div twoColsStyle
                    [ viewMem mem
                    , Html.div [] [ Html.text "Disassembly unavailable" ]
                    ]
                ]

            ParsedAndDisassembled ( string, mem, disassembled ) ->
                [ viewString string
                , Html.div twoColsStyle
                    [ viewMem mem
                    , viewDisassembled disassembled
                    ]
                ]
    }


viewString : String -> Html Msg
viewString string =
    Html.textarea
        [ Events.onInput SetRawProgram
        , Attrs.cols 80
        , Attrs.rows 10
        , Attrs.placeholder "Paste your memory (puzzle input) here!"
        , Attrs.value string
        ]
        []


viewMem : Memory -> Html Msg
viewMem mem =
    Html.pre
        []
        [ mem
            |> Array.toList
            |> List.indexedMap (\i n -> "[" ++ String.fromInt i ++ "]: " ++ String.fromInt n)
            |> String.join "\n"
            |> Html.text
        ]


paramToString : Parameter -> String
paramToString param =
    "("
        ++ (case param of
                Position n ->
                    "Position " ++ String.fromInt n

                Immediate n ->
                    "Immediate " ++ String.fromInt n
           )
        ++ ")"


viewDisassembled : List ( Int, Data Op ) -> Html Msg
viewDisassembled data =
    Html.pre
        []
        [ data
            |> List.map
                (\( position, datum ) ->
                    "["
                        ++ String.fromInt position
                        ++ "]: "
                        ++ (case datum of
                                Data n ->
                                    "raw data " ++ String.fromInt n

                                Instruction op ->
                                    case op of
                                        Add { addr0, addr1, dest } ->
                                            "Add " ++ paramToString addr0 ++ " " ++ paramToString addr1 ++ " " ++ paramToString dest

                                        Mult { addr0, addr1, dest } ->
                                            "Mult " ++ paramToString addr0 ++ " " ++ paramToString addr1 ++ " " ++ paramToString dest

                                        SetInputAt { dest } ->
                                            "SetInputAt " ++ paramToString dest

                                        Print { addr } ->
                                            "Print " ++ paramToString addr

                                        JumpIfTrue { test, jumpTo } ->
                                            "JumpIfTrue " ++ paramToString test ++ " " ++ paramToString jumpTo

                                        JumpIfFalse { test, jumpTo } ->
                                            "JumpIfFalse " ++ paramToString test ++ " " ++ paramToString jumpTo

                                        LessThan { left, right, dest } ->
                                            "LessThan " ++ paramToString left ++ " " ++ paramToString right ++ " " ++ paramToString dest

                                        Equals { left, right, dest } ->
                                            "Equals " ++ paramToString left ++ " " ++ paramToString right ++ " " ++ paramToString dest

                                        Halt ->
                                            "Halt"
                           )
                )
            |> String.join "\n"
            |> Html.text
        ]
