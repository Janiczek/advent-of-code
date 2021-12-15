module Year2017.Day1X exposing (..)

import Advent exposing (Test)
import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Fifo exposing (Fifo)


main : Program Never ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input
        , parse1 = parse
        , parse2 = parse
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


type alias Input =
    Array Inst


type alias Output1 =
    Int


type alias Output2 =
    ( Int, Int )


parse : String -> Input
parse input =
    input
        |> String.lines
        |> List.map parseLine
        |> Array.fromList


parseLine : String -> Inst
parseLine line =
    case String.words line of
        [ "set", reg, valOrReg ] ->
            Set reg (toIntOrVal valOrReg)

        [ "add", reg, valOrReg ] ->
            Add reg (toIntOrVal valOrReg)

        [ "mul", reg, valOrReg ] ->
            Mul reg (toIntOrVal valOrReg)

        [ "mod", reg, valOrReg ] ->
            Mod reg (toIntOrVal valOrReg)

        [ "snd", valOrReg ] ->
            Snd (toIntOrVal valOrReg)

        [ "rcv", reg ] ->
            Rcv reg

        [ "jgz", valOrReg1, valOrReg2 ] ->
            Jgz (toIntOrVal valOrReg1) (toIntOrVal valOrReg2)

        _ ->
            Debug.crash <| "wrong input! " ++ line


toIntOrVal : String -> Content
toIntOrVal string =
    case String.toInt string of
        Ok int ->
            V int

        Err _ ->
            R string


compute1 : Input -> Output1
compute1 input =
    step input (initMachine 0)


step : Array Inst -> Machine -> Int
step insts machine =
    -- ends when `rcv` doesnt get 0
    let
        inst =
            insts
                |> Array.get machine.current
                |> Advent.unsafeMaybe
    in
        case inst of
            Set reg content ->
                step insts (set reg content machine)

            Add reg content ->
                step insts (add reg content machine)

            Mul reg content ->
                step insts (mul reg content machine)

            Mod reg content ->
                step insts (mod reg content machine)

            Jgz content1 content2 ->
                step insts (jgz content1 content2 machine)

            Snd content ->
                step insts (snd content machine)

            Rcv reg ->
                let
                    val =
                        get reg machine.regs
                in
                    if val > 0 then
                        machine.lastPlayedFrequency
                    else
                        step insts (incrCurrent machine)


set : Reg -> Content -> Machine -> Machine
set reg content machine =
    { machine
        | regs =
            machine.regs
                |> Dict.update reg
                    (content
                        |> toValue machine.regs
                        |> Just
                        |> always
                    )
        , current = machine.current + 1
    }


add : Reg -> Content -> Machine -> Machine
add reg content machine =
    { machine
        | regs =
            machine.regs
                |> Dict.update reg
                    (\maybeVal ->
                        maybeVal
                            |> Maybe.withDefault 0
                            |> (\v -> v + toValue machine.regs content)
                            |> Just
                    )
        , current = machine.current + 1
    }


mul : Reg -> Content -> Machine -> Machine
mul reg content machine =
    { machine
        | regs =
            machine.regs
                |> Dict.update reg
                    (\maybeVal ->
                        maybeVal
                            |> Maybe.withDefault 0
                            |> (\v -> v * toValue machine.regs content)
                            |> Just
                    )
        , current = machine.current + 1
    }


mod : Reg -> Content -> Machine -> Machine
mod reg content machine =
    { machine
        | regs =
            machine.regs
                |> Dict.update reg
                    (\maybeVal ->
                        maybeVal
                            |> Maybe.withDefault 0
                            |> (\v -> v % toValue machine.regs content)
                            |> Just
                    )
        , current = machine.current + 1
    }


jgz : Content -> Content -> Machine -> Machine
jgz content1 content2 machine =
    { machine
        | current =
            if (toValue machine.regs content1) > 0 then
                machine.current + (toValue machine.regs content2)
            else
                machine.current + 1
    }


snd : Content -> Machine -> Machine
snd content machine =
    { machine
        | lastPlayedFrequency = get (toReg content) machine.regs
        , current = machine.current + 1
    }


incrCurrent : Machine -> Machine
incrCurrent machine =
    { machine | current = machine.current + 1 }


get : Reg -> Regs -> Int
get reg regs =
    regs
        |> Dict.get reg
        |> Maybe.withDefault 0


toReg : Content -> Reg
toReg content =
    case content of
        R reg ->
            reg

        V val ->
            Debug.crash "snd with a number in part 1, AAAAAAARGGGGGGGHHHHHH"


toValue : Regs -> Content -> Int
toValue regs content =
    case content of
        R reg ->
            get reg regs

        V val ->
            val


showContent : Regs -> Content -> String
showContent regs content =
    case content of
        R reg ->
            reg ++ " (" ++ toString (get reg regs) ++ ")"

        V val ->
            toString val


type alias Regs =
    Dict String Int


type alias Machine =
    { regs : Dict String Int
    , current : Int
    , lastPlayedFrequency : Int
    , sendingQueue : Fifo Int
    , waitsForDataOnReg : Maybe Reg
    , sendCount : Int
    , id : Int
    }


initMachine : Int -> Machine
initMachine id =
    Machine
        ([ ( "p", id ) ] |> Dict.fromList)
        0
        -1
        Fifo.empty
        Nothing
        0
        id


compute2 : Input -> Output2
compute2 input =
    step2 input ( initMachine 0, initMachine 1 )


type alias Machines =
    ( Machine, Machine )


step2 : Array Inst -> Machines -> ( Int, Int )
step2 insts (( machine1, machine2 ) as machines) =
    -- both wait
    if isDeadlock machines then
        ( machine1.sendCount, machine2.sendCount )
        -- m1 can run
    else if machine1.waitsForDataOnReg == Nothing then
        step2 insts ( run insts machine1, machine2 )
        -- m2 can run
    else if machine2.waitsForDataOnReg == Nothing then
        step2 insts ( machine1, run insts machine2 )
        -- send from 2 to 1
    else if machine2.sendingQueue /= Fifo.empty then
        let
            ( newMachine2, newMachine1 ) =
                sendFrom machine2 machine1
        in
            step2 insts ( newMachine1, newMachine2 )
        -- send from 1 to 2
    else if machine1.sendingQueue /= Fifo.empty then
        let
            ( newMachine1, newMachine2 ) =
                sendFrom machine1 machine2
        in
            step2 insts ( newMachine1, newMachine2 )
    else
        Debug.crash "?"


run : Array Inst -> Machine -> Machine
run insts machine =
    let
        inst =
            insts
                |> Array.get machine.current
                |> Advent.unsafeMaybe

        _ =
            Debug.log (toString machine.id) (show machine.regs inst)
    in
        case inst of
            Set reg content ->
                set reg content machine

            Add reg content ->
                add reg content machine

            Mul reg content ->
                mul reg content machine

            Mod reg content ->
                mod reg content machine

            Jgz reg content ->
                jgz reg content machine

            Snd content ->
                send content machine

            Rcv reg ->
                recv reg machine


show : Dict String Int -> Inst -> String
show regs inst =
    (case inst of
        Set reg content ->
            reg ++ " = " ++ showContent regs content

        Add reg content ->
            reg ++ " += " ++ showContent regs content

        Mul reg content ->
            reg ++ " *= " ++ showContent regs content

        Mod reg content ->
            reg ++ " %= " ++ showContent regs content

        Jgz content1 content2 ->
            "jump " ++ (showContent regs content2) ++ " if " ++ (showContent regs content1) ++ " > 0"

        Snd content ->
            "send " ++ (showContent regs content)

        Rcv reg ->
            "receive to reg " ++ reg
    )
        ++ showRegs regs


showRegs : Dict String Int -> String
showRegs regs =
    " /// "
        ++ (regs
                |> Dict.toList
                |> List.map (\( reg, val ) -> reg ++ ": " ++ toString val)
                |> String.join ", "
           )


send : Content -> Machine -> Machine
send content machine =
    { machine
        | sendingQueue =
            machine.sendingQueue
                |> Fifo.insert (toValue machine.regs content)
        , current = machine.current + 1
    }


recv : Reg -> Machine -> Machine
recv reg machine =
    { machine
        | waitsForDataOnReg = Just reg
        , current = machine.current + 1
    }


sendFrom : Machine -> Machine -> ( Machine, Machine )
sendFrom machineA machineB =
    let
        _ =
            Debug.log ("can send from " ++ (toString machineA.id) ++ " to " ++ (toString machineB.id)) val

        ( maybeVal, queueA ) =
            Fifo.remove machineA.sendingQueue

        val =
            maybeVal
                |> Advent.unsafeMaybe

        newA =
            { machineA
                | sendingQueue = queueA
                , sendCount = machineA.sendCount + 1
            }

        newB =
            machineB |> receive val
    in
        ( newA, newB )


receive : Int -> Machine -> Machine
receive val machine =
    case machine.waitsForDataOnReg of
        Nothing ->
            Debug.crash "receiving but not waiting!"

        Just reg ->
            { machine
                | regs =
                    machine.regs
                        |> Dict.update reg (always (Just val))
                , waitsForDataOnReg = Nothing
            }


isDeadlock : Machines -> Bool
isDeadlock ( m1, m2 ) =
    (m1.waitsForDataOnReg /= Nothing)
        && (m2.waitsForDataOnReg /= Nothing)
        && (m1.sendingQueue == Fifo.empty)
        && (m2.sendingQueue == Fifo.empty)


type alias Reg =
    String


type Content
    = R Reg
    | V Int


type Inst
    = Set Reg Content
    | Add Reg Content
    | Mul Reg Content
    | Mod Reg Content
    | Snd Content
    | Rcv Reg
    | Jgz Content Content


tests1 : List (Test Input Output1)
tests1 =
    [ Test "example"
        """set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"""
        ([ Set "a" (V 1)
         , Add "a" (V 2)
         , Mul "a" (R "a")
         , Mod "a" (V 5)
         , Snd (R "a")
         , Set "a" (V 0)
         , Rcv "a"
         , Jgz (R "a") (V -1)
         , Set "a" (V 1)
         , Jgz (R "a") (V -2)
         ]
            |> Array.fromList
        )
        4
    ]


tests2 : List (Test Input Output2)
tests2 =
    [ Test "example"
        """snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d"""
        ([ Snd (V 1)
         , Snd (V 2)
         , Snd (R "p")
         , Rcv "a"
         , Rcv "b"
         , Rcv "c"
         , Rcv "d"
         ]
            |> Array.fromList
        )
        ( 3, 3 )
    ]


input : String
input =
    """set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 618
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19"""
