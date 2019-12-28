module Year2019.Intcode.Disasm.Main exposing (main)

import Array exposing (Array)
import Browser
import Dict
import Html exposing (Attribute, Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Events.Extra as Events
import Json.Decode as Decode
import Maybe.Extra
import Year2019.Intcode as Intcode exposing (Mask(..), Op(..))
import Year2019.Intcode.Disasm as Disasm exposing (Data(..))
import Year2019.Intcode.Memory as Memory exposing (Memory)
import Year2019.Intcode.Parameter as Parameter exposing (Parameter(..))


type alias Model =
    { rawProgram : String
    , memory : Maybe Memory
    , disasm : Maybe (List ( Int, Data ))
    , decompiled : Maybe (List ( Int, String ))
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
    ( { rawProgram = ""
      , memory = Nothing
      , disasm = Nothing
      , decompiled = Nothing
      }
    , Cmd.none
    )


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
    , Cmd.none
    )


recalc : Model -> Model
recalc model =
    let
        maybeMem : Maybe Memory
        maybeMem =
            Memory.fromString model.rawProgram

        maybeDisassembled : Maybe (List ( Int, Data ))
        maybeDisassembled =
            Maybe.map Disasm.disassemble maybeMem

        maybeDecompiled : Maybe (List ( Int, String ))
        maybeDecompiled =
            Maybe.map (List.map (Tuple.mapSecond decompile)) maybeDisassembled
    in
    { model
        | memory = maybeMem
        , disasm = maybeDisassembled
        , decompiled = maybeDecompiled
    }


m : Int -> String
m n =
    "m[" ++ String.fromInt n ++ "]"


param : Parameter -> String
param p =
    case p of
        Immediate n ->
            String.fromInt n

        Position pos ->
            m pos

        Relative relPos ->
            "m[rel + " ++ String.fromInt relPos ++ "]"


decompile : Data -> String
decompile data =
    case data of
        Instruction op ->
            case op of
                Add { addr0, addr1, dest } ->
                    if dest == addr0 then
                        m (Intcode.unwrapParam dest) ++ " += " ++ param addr1

                    else if dest == addr1 then
                        m (Intcode.unwrapParam dest) ++ " += " ++ param addr0

                    else
                        m (Intcode.unwrapParam dest) ++ " = " ++ param addr0 ++ " + " ++ param addr1

                Mult { addr0, addr1, dest } ->
                    if dest == addr0 then
                        m (Intcode.unwrapParam dest) ++ " *= " ++ param addr1

                    else if dest == addr1 then
                        m (Intcode.unwrapParam dest) ++ " *= " ++ param addr0

                    else
                        m (Intcode.unwrapParam dest) ++ " = " ++ param addr0 ++ " * " ++ param addr1

                Input { dest } ->
                    m (Intcode.unwrapParam dest) ++ " = get_input()"

                Output { addr } ->
                    "print(" ++ param addr ++ ")"

                JumpIfTrue { test, jumpPosition } ->
                    case test of
                        Immediate 0 ->
                            "do_nothing()"

                        Immediate _ ->
                            "goto " ++ param jumpPosition

                        Position pos ->
                            "if " ++ m pos ++ " /= 0:\n    goto " ++ param jumpPosition

                        Relative relPos ->
                            "if m[rel + " ++ String.fromInt relPos ++ "] /= 0:\n    goto " ++ param jumpPosition

                JumpIfFalse { test, jumpPosition } ->
                    case test of
                        Immediate 0 ->
                            "goto " ++ param jumpPosition

                        Immediate _ ->
                            "do_nothing()"

                        Position pos ->
                            "if " ++ m pos ++ " == 0:\n    goto " ++ param jumpPosition

                        Relative relPos ->
                            "if m[rel + " ++ String.fromInt relPos ++ "] == 0:\n    goto " ++ param jumpPosition

                LessThan { left, right, dest } ->
                    m (Intcode.unwrapParam dest)
                        ++ " = "
                        ++ (case ( left, right ) of
                                ( Immediate left_, Immediate right_ ) ->
                                    if left_ < right_ then
                                        "1"

                                    else
                                        "0"

                                _ ->
                                    "(1 if " ++ param left ++ " < " ++ param right ++ " else 0)"
                           )

                Equals { left, right, dest } ->
                    m (Intcode.unwrapParam dest)
                        ++ " = "
                        ++ (case ( left, right ) of
                                ( Immediate left_, Immediate right_ ) ->
                                    if left_ == right_ then
                                        "1"

                                    else
                                        "0"

                                _ ->
                                    "(1 if " ++ param left ++ " == " ++ param right ++ " else 0)"
                           )

                AddToRelativeBase { value } ->
                    "addToRelativeBase(" ++ param value ++ ")"

                Halt ->
                    "halt()"

        Data n ->
            "raw data " ++ String.fromInt n


colsStyle : List (Attribute Msg)
colsStyle =
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
        , Html.div colsStyle
            [ viewMem model.memory
            , viewDisassembled model.disasm
            , viewDecompiled model.decompiled
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


viewMem : Maybe Memory -> Html Msg
viewMem maybeMem =
    maybeMem
        |> Maybe.map
            (\mem ->
                Html.div colStyle
                    [ Html.h2 [] [ Html.text "Memory" ]
                    , Html.pre
                        []
                        [ [ mem.program
                                |> Array.toList
                                |> List.indexedMap (\i n -> "[" ++ String.fromInt i ++ "]: " ++ String.fromInt n)
                          , [ "--------------"
                            , "Extra:"
                            ]
                          , mem.extra
                                |> Dict.toList
                                |> List.map (\( i, n ) -> "[" ++ String.fromInt i ++ "]: " ++ String.fromInt n)
                          ]
                            |> List.concat
                            |> String.join "\n"
                            |> Html.text
                        ]
                    ]
            )
        |> Maybe.withDefault (Html.div colStyle [ Html.text "Memory unavailable" ])


paramToString : Parameter -> String
paramToString param_ =
    "("
        ++ (case param_ of
                Position n ->
                    "Position " ++ String.fromInt n

                Immediate n ->
                    "Immediate " ++ String.fromInt n

                Relative n ->
                    "Relative " ++ String.fromInt n
           )
        ++ ")"


viewDisassembled : Maybe (List ( Int, Data )) -> Html Msg
viewDisassembled maybeData =
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

                                                        Input { dest } ->
                                                            "Input " ++ paramToString dest

                                                        Output { addr } ->
                                                            "Output " ++ paramToString addr

                                                        JumpIfTrue { test, jumpPosition } ->
                                                            "JumpIfTrue " ++ paramToString test ++ " " ++ paramToString jumpPosition

                                                        JumpIfFalse { test, jumpPosition } ->
                                                            "JumpIfFalse " ++ paramToString test ++ " " ++ paramToString jumpPosition

                                                        LessThan { left, right, dest } ->
                                                            "LessThan " ++ paramToString left ++ " " ++ paramToString right ++ " " ++ paramToString dest

                                                        Equals { left, right, dest } ->
                                                            "Equals " ++ paramToString left ++ " " ++ paramToString right ++ " " ++ paramToString dest

                                                        AddToRelativeBase { value } ->
                                                            "AddToRelativeBase " ++ paramToString value

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


viewDecompiled : Maybe (List ( Int, String )) -> Html Msg
viewDecompiled maybeDecompiled =
    maybeDecompiled
        |> Maybe.map
            (\statements ->
                Html.div colStyle
                    [ Html.h2 [] [ Html.text "Decompiled program" ]
                    , Html.p [] [ Html.text "DISCLAIMER: Intcode programs might be self-modifying. This won't be accurate for all inputs." ]
                    , Html.ul
                        [ Attrs.style "margin-top" "16px"
                        , Attrs.style "font-family" "monospace"
                        ]
                      <|
                        List.map viewStatement statements
                    ]
            )
        |> Maybe.withDefault (Html.div colStyle [ Html.text "Decompiled program unavailable" ])


viewStatement : ( Int, String ) -> Html Msg
viewStatement ( position, statement ) =
    Html.li
        [ Attrs.style "display" "flex"
        , Attrs.style "flex-direction" "row"
        ]
        [ Html.div
            [ Attrs.style "color" "#888888"
            , Attrs.style "width" "40px"
            , Attrs.style "margin-right" "8px"
            ]
            [ Html.text <| m position ]
        , Html.pre [] [ Html.text statement ]
        ]
