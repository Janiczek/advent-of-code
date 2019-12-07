module Year2019.Intcode.Disasm.Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Attribute, Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Events.Extra as Events
import Json.Decode as Decode
import Maybe.Extra
import Year2019.Intcode as Intcode exposing (Mask(..), Memory, Parameter(..))
import Year2019.Intcode.Disasm as Disasm exposing (Data(..))


type IntcodeProgram
    = UnsuccessfulParse
    | Parsed Memory
    | ParsedAndDisassembled ( Memory, List ( Int, Data Op ) )


type alias Model =
    { rawProgram : String
    , memory : Maybe Memory
    , disasm : Maybe (List ( Int, Data Op ))
    , offset : Int
    , ops : List ( Int, Intcode.Op Op )
    }


type Msg
    = SetRawProgram String
    | SetOffset Int


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
    ( { rawProgram = ""
      , memory = Nothing
      , disasm = Nothing
      , ops = supportedOps -- TODO allow the user to change this
      , offset = 0
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
                | rawProgram =
                    string
                        |> String.replace "\n" ""
                        |> String.replace "\t" ""
                        |> String.replace " " ""
            }
                |> recalc

        SetOffset offset ->
            { model | offset = offset }
                |> recalc
    , Cmd.none
    )


dropArray : Int -> Array a -> Array a
dropArray n array =
    Array.slice n (Array.length array) array


recalc : Model -> Model
recalc model =
    let
        maybeMem : Maybe Memory
        maybeMem =
            Intcode.parseSafe model.rawProgram

        maybeDisassembled : Maybe (List ( Int, Data Op ))
        maybeDisassembled =
            Maybe.map
                (\mem -> Disasm.disassembleWith model.ops (dropArray model.offset mem))
                maybeMem
    in
    { model
        | memory = maybeMem
        , disasm = maybeDisassembled
    }


twoColsStyle : List (Attribute Msg)
twoColsStyle =
    [ Attrs.style "display" "flex"
    , Attrs.style "flex-direction" "row"
    ]


colStyle : List (Attribute Msg)
colStyle =
    [ Attrs.style "margin-right" "8px"
    , Attrs.style "padding" "8px"
    , Attrs.style "background-color" "#eeeeee"
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Intcode Disassembler"
    , body =
        [ viewHeader
        , viewTextarea model.rawProgram
        , viewOffsetSlider model.offset
        , Html.div twoColsStyle
            [ viewMem model.memory
            , viewDisassembled model.offset model.disasm
            ]
        ]
    }


viewHeader : Html Msg
viewHeader =
    Html.h1 [] [ Html.text "Intcode Disassembler" ]


viewTextarea : String -> Html Msg
viewTextarea string =
    Html.textarea
        [ Events.onInput SetRawProgram
        , Attrs.cols 80
        , Attrs.rows 10
        , Attrs.placeholder "Paste your memory (puzzle input) here!"
        , Attrs.value string
        ]
        []


viewOffsetSlider : Int -> Html Msg
viewOffsetSlider offset =
    let
        stringOffset =
            String.fromInt offset
    in
    Html.div []
        [ Html.input
            [ Attrs.type_ "range"
            , Attrs.min "0"
            , Attrs.max "3"
            , Attrs.value stringOffset
            , Events.on "input" (Decode.map SetOffset Events.targetValueInt)
            ]
            []
        , Html.text <| "Current offset: " ++ stringOffset
        ]


viewMem : Maybe Memory -> Html Msg
viewMem maybeMem =
    maybeMem
        |> Maybe.map
            (\mem ->
                Html.div colStyle
                    [ Html.h2 [] [ Html.text "Memory" ]
                    , Html.pre
                        []
                        [ mem
                            |> Array.toList
                            |> List.indexedMap (\i n -> "[" ++ String.fromInt i ++ "]: " ++ String.fromInt n)
                            |> String.join "\n"
                            |> Html.text
                        ]
                    ]
            )
        |> Maybe.withDefault (Html.div colStyle [ Html.text "Memory unavailable" ])


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


viewDisassembled : Int -> Maybe (List ( Int, Data Op )) -> Html Msg
viewDisassembled offset maybeData =
    maybeData
        |> Maybe.map
            (\data ->
                Html.div colStyle
                    [ Html.h2 [] [ Html.text "Disassembled program" ]
                    , Html.pre
                        []
                        [ data
                            |> List.map
                                (\( position, datum ) ->
                                    "["
                                        ++ String.fromInt (position + offset)
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
                    ]
            )
        |> Maybe.withDefault (Html.div colStyle [ Html.text "Disassembly unavailable" ])
