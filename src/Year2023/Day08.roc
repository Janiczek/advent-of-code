app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "../../input202308.txt" as realInput : Str
    ]
    provides [main] to pf


#main = part1 realInput
main = part2 realInput
#main = part1 testInput
#main = part2 testInput

Input: {
    steps: List [L,R],
    nodes: Dict Str {l: Str, r: Str},
}

parse : Str -> Input
parse = \str ->
    when Str.split str "\n\n" is
        [stepsStr, nodesStr] ->
            steps = 
                stepsStr
                |> Str.graphemes
                |> List.keepOks (\s ->
                    when s is
                        "L" -> Ok L
                        "R" -> Ok R
                        _ -> Err UnknownStep
                )

            nodes =
                nodesStr
                |> Str.split "\n"
                |> List.dropIf (\s -> s == "")
                |> List.map parseNode
                |> Dict.fromList

            { 
                steps: steps,
                nodes: nodes,
            }

        _ ->
            crash "parse: weird input"


dropEnd : Str,Nat -> Str
dropEnd = \s,n ->
    s
    |> Str.graphemes
    |> List.dropLast n
    |> List.walk "" Str.concat

parseNode : Str -> (Str, {l: Str, r: Str})
parseNode = \line ->
    when line |> dropEnd 1 |> Str.split " = (" is
        [startNode, endNodes] ->
            when Str.split endNodes ", " is
                [left, right] -> (startNode, {l: left, r: right})
                _ -> crash "parseNode: unexpected end nodes"
        _ -> crash "parseNode: unexpected line"

##### PART 1 ######

part1 = \input ->
    input
    |> parse
    |> (\i -> go1 0 "AAA" i.steps i)
    |> Inspect.toStr
    |> Stdout.line

go1 : I64,Str,List [L,R],Input -> I64
go1 = \n,current,todoSteps,input ->
    if current == "ZZZ" then
        n
    else
        when todoSteps is
            [] ->
                go1 n current input.steps input
            [step, .. as restSteps] ->
                when Dict.get input.nodes current is
                    Ok next ->
                        when step is
                            L -> go1 (n+1) next.l restSteps input
                            R -> go1 (n+1) next.r restSteps input

                    Err KeyNotFound ->
                        crash "go1: wut"


##### PART 2 ######

part2 = \input ->
    input
    |> part2Pure
    |> Inspect.toStr
    |> Stdout.line

part2Pure = \input ->
    i = parse input

    startNodes = 
        i.nodes
        |> Dict.toList
        |> List.keepIf (\(node,_) -> node |> Str.endsWith "A")
        |> List.map (\(node,_) -> node)

    cycles =
        startNodes
        |> List.map (\node -> findCycleLength node 0 NotSeen i.steps i)

    finalCycle = 
        cycles 
        |> List.walk {phase:0,period:1} combineCycles

    finalCycle.period

testInput =
    "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)"

expect part2Pure testInput == 6

lcmList : List I64 -> I64
lcmList = \list ->
    List.walk list 1 lcm

lcm : I64,I64 -> I64
lcm = \a,b ->
    Num.abs (a * b) // gcd a b 

gcd : I64,I64 -> I64
gcd = \a,b ->
    if b == 0 then
        a
    else
        gcd b (a % b)

extendedGcd : I64,I64 -> {gcd:I64,s:I64,t:I64}
extendedGcd = \a,b ->
    if b == 0 then
        {gcd:a, s:1, t:0}
    else
        q = a // b
        r = a % b
        g = extendedGcd b r
        {
            gcd: gcd a b,
            s: g.t,
            t: g.s-q*g.t,
        }

expect extendedGcd 2 3 == {gcd:1,s:-1,t:1}

Cycle: {
    period: I64,
    phase: I64,
}
combineCycles : Cycle,Cycle -> Cycle
combineCycles = \a,b ->
    g = extendedGcd a.period b.period
    phaseDiff = a.phase - b.phase
    if phaseDiff % g.gcd != 0 then
        crash "they will never align"
    else
        z = phaseDiff // g.gcd
        m = z * g.s
        cPeriod = lcm a.period b.period
        cPhase = (((-m * a.period + a.phase) % cPeriod) + cPeriod) % cPeriod
        { 
            phase: cPhase,
            period: cPeriod,
        }

expect (combineCycles {period:9,phase:0} {period:15,phase:3}).phase == 18
expect (combineCycles {period:30,phase:0} {period:38,phase:6}).phase == 120

crashResult : Result a *, Str -> a
crashResult = \result,label ->
    when result is
        Ok a -> a
        Err _ -> crash label

findCycleLength : Str,I64,[NotSeen,Seen I64],List [L,R],Input -> Cycle
findCycleLength = \current,counter,state,todoSteps,input ->
    when todoSteps is
        [] ->
            findCycleLength 
                current
                counter
                state
                input.steps
                input

        [step, .. as restSteps] ->

            if current |> Str.endsWith "Z" then
                when state is
                    NotSeen -> 
                        nextNode = 
                            when Dict.get input.nodes current is
                                Ok next ->
                                    when step is 
                                        L -> next.l
                                        R -> next.r
                                Err KeyNotFound ->
                                    crash "go2: wut"

                        nextState = Seen counter

                        findCycleLength
                            nextNode
                            (counter + 1)
                            nextState
                            restSteps
                            input

                    Seen firstZ -> 
                        # BASE CASE
                        {
                            phase: firstZ,
                            period: counter - firstZ,
                        }

            else
                nextNode = 
                    when Dict.get input.nodes current is
                        Ok next ->
                            when step is 
                                L -> next.l
                                R -> next.r
                        Err KeyNotFound ->
                            crash "go2: wut"

                findCycleLength
                    nextNode
                    (counter + 1)
                    state
                    restSteps
                    input
