app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "../../input202307.txt" as realInput : Str
    ]
    provides [main] to pf


#main = part1 realInput
main = part2 realInput
#main = part1 testInput
#main = part2 testInput

testInput : Str
testInput =
    "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

cardValue : Str -> Nat
cardValue = \char ->
    when char is
        "A" -> 13
        "K" -> 12
        "Q" -> 11
        "J" -> 10
        "T" -> 9
        "9" -> 8
        "8" -> 7
        "7" -> 6
        "6" -> 5
        "5" -> 4
        "4" -> 3
        "3" -> 2
        "2" -> 1
        _ -> crash "weird card value"

Category: [
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
]

frequencies : List a -> Dict a Nat
frequencies = \list ->
    list
    |> List.walk
        (Dict.empty {}) 
        (\counter,el ->
            current = 
                counter
                |> Dict.get el
                |> Result.withDefault 0

            counter
            |> Dict.insert el (current + 1)
        )


invert : Dict a b -> Dict b (List a)
invert = \dict ->
    dict
    |> Dict.toList
    |> List.map (\(k,v) -> (v,k))
    |> List.walk
        (Dict.empty {})
        (\acc,(k,v) ->
            acc
            |> Dict.update k
                (\maybeV ->
                    when maybeV is
                        Missing -> Present [v]
                        Present vs -> Present (List.append vs v)
                )
        )

categorize : Str -> Category
categorize = \hand ->

    counts : Dict Nat (List Str)
    counts =
        hand
            |> Str.graphemes
            |> frequencies
            |> invert

    if Dict.contains counts 5 then
        FiveOfAKind
    else if Dict.contains counts 4 then
        FourOfAKind
    else if Dict.contains counts 3 && Dict.contains counts 2 then
        FullHouse
    else if Dict.contains counts 3 then
        ThreeOfAKind
    else if Dict.get counts 2 |> Result.map List.len == Ok 2 then
        TwoPair
    else if Dict.contains counts 2 then
        OnePair
    else
        HighCard

categoryValue : Category -> Nat
categoryValue = \cat ->
    when cat is
        FiveOfAKind  -> 7
        FourOfAKind  -> 6
        FullHouse    -> 5
        ThreeOfAKind -> 4
        TwoPair      -> 3
        OnePair      -> 2
        HighCard     -> 1

compareHand : Str, Str -> [LT,EQ,GT]
compareHand = \a,b ->
    catA = categorize a
    catB = categorize b
    when Num.compare (categoryValue catA) (categoryValue catB) is
        LT -> LT
        GT -> GT
        EQ -> 
            cardsA = a |> Str.graphemes |> List.map cardValue
            cardsB = b |> Str.graphemes |> List.map cardValue
            compareList cardsA cardsB

    
compareList : List Nat, List Nat -> [LT,EQ,GT]
compareList = \a,b ->
    when (a,b) is
        ([],[]) -> EQ
        (_,[]) -> GT
        ([],_) -> LT
        ([anA, .. as restA],[anB, .. as restB]) -> 
            when Num.compare anA anB is
                LT -> LT
                GT -> GT
                EQ -> compareList restA restB

compareLine : {hand: Str, bid: Nat},{hand: Str, bid: Nat} -> [LT,EQ,GT]
compareLine = \a,b ->
    compareHand a.hand b.hand

parse : Str -> List {hand: Str, bid: Nat}
parse = \input ->
    input
    |> Str.split "\n"
    |> List.dropIf (\s -> s == "")
    |> List.map parseLine

parseLine : Str -> {hand: Str, bid: Nat}
parseLine = \line ->
    when Str.split line " " is
        [hand, bidStr] ->
            {
                hand: hand,
                bid: Str.toNat bidStr |> crashResult "bid not int",
            }
        _ ->
            crash "weird parseLine"

crashResult : Result a *, Str -> a
crashResult = \result,label ->
    when result is
        Ok a -> a
        Err _ -> crash label

##### PART 1 #####

expect part1Pure testInput == 6440

part1Pure : Str -> Nat
part1Pure = \input ->
    input
    |> parse
    |> List.sortWith compareLine
    |> List.mapWithIndex (\line,i -> line.bid * (i+1))
    |> List.sum

part1 = \input ->
    input
    |> part1Pure
    |> Inspect.toStr
    |> Stdout.line

######### PART 2 #########

part2 = \input ->
    input
    |> parse
    |> List.sortWith compareLineWithJokers
    |> List.mapWithIndex (\line,i -> line.bid * (i+1))
    |> List.sum
    |> Inspect.toStr
    |> Stdout.line

compareLineWithJokers : {hand: Str, bid: Nat},{hand: Str, bid: Nat} -> [LT,EQ,GT]
compareLineWithJokers = \a,b ->
    compareHandWithJokers a.hand b.hand

compareHandWithJokers : Str, Str -> [LT,EQ,GT]
compareHandWithJokers = \a,b ->
    catA = categorizeWithJokers a
    catB = categorizeWithJokers b
    when Num.compare (categoryValue catA) (categoryValue catB) is
        LT -> LT
        GT -> GT
        EQ -> 
            cardsA = a |> Str.graphemes |> List.map cardValueWithJokers
            cardsB = b |> Str.graphemes |> List.map cardValueWithJokers
            compareList cardsA cardsB

allCardsWithoutJ : List Str
allCardsWithoutJ =
    Str.graphemes "AKQT98765432"
            
categorizeWithJokers : Str -> Category
categorizeWithJokers = \hand ->
    allCardsWithoutJ
    |> List.map (\card -> 
        hand 
        |> replaceJWith card
        |> categorize
    )
    |> List.sortWith (\a,b -> Num.compare (categoryValue a) (categoryValue b))
    |> List.last
    |> crashResult "categorize with jokers - list.last"

replaceJWith : Str, Str -> Str
replaceJWith = \hand,card ->
    hand
    |> Str.replaceEach "J" card

cardValueWithJokers : Str -> Nat
cardValueWithJokers = \char ->
    when char is
        "A" -> 13
        "K" -> 12
        "Q" -> 11
        "T" -> 10
        "9" -> 9
        "8" -> 8
        "7" -> 7
        "6" -> 6
        "5" -> 5
        "4" -> 4
        "3" -> 3
        "2" -> 2
        "J" -> 1
        _ -> crash "weird card value (with jokers)"
