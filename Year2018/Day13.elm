module Year2018.Day13 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import List.Extra



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    ( Tracks, Carts )


type alias Input2 =
    ( Tracks, Carts )


type alias Output1 =
    String


type alias Output2 =
    String


type Track
    = Vertical
    | Horizontal
    | DiagonalToRight
    | DiagonalToLeft
    | Intersection


type alias Tracks =
    Dict ( Int, Int ) Track


type alias Carts =
    List Cart


type Turn
    = TurnLeft
    | TurnStraight
    | TurnRight


type Direction
    = Right
    | Left
    | Up
    | Down


type Cart
    = WorkingCart
        { position : ( Int, Int )
        , direction : Direction
        , nextIntersection : Turn
        }
    | CrashedCart ( Int, Int )



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    ( parseTracks string
    , parseCarts string
    )


parseTracks : String -> Tracks
parseTracks string =
    string
        |> String.lines
        |> List.indexedMap
            (\y row ->
                row
                    |> String.toList
                    |> List.indexedMap
                        (\x char ->
                            case char of
                                '/' ->
                                    Just ( ( x, y ), DiagonalToRight )

                                '\\' ->
                                    Just ( ( x, y ), DiagonalToLeft )

                                '|' ->
                                    Just ( ( x, y ), Vertical )

                                '-' ->
                                    Just ( ( x, y ), Horizontal )

                                '+' ->
                                    Just ( ( x, y ), Intersection )

                                '>' ->
                                    Just ( ( x, y ), Horizontal )

                                '<' ->
                                    Just ( ( x, y ), Horizontal )

                                'v' ->
                                    Just ( ( x, y ), Vertical )

                                '^' ->
                                    Just ( ( x, y ), Vertical )

                                ' ' ->
                                    Nothing

                                other ->
                                    Debug.todo ("wrong input: " ++ String.fromChar other)
                        )
            )
        |> List.concat
        |> List.filterMap identity
        |> Dict.fromList


parseCarts : String -> Carts
parseCarts string =
    string
        |> String.lines
        |> List.indexedMap
            (\y row ->
                row
                    |> String.toList
                    |> List.indexedMap
                        (\x char ->
                            case char of
                                '>' ->
                                    Just ( x, y, Right )

                                '<' ->
                                    Just ( x, y, Left )

                                'v' ->
                                    Just ( x, y, Down )

                                '^' ->
                                    Just ( x, y, Up )

                                other ->
                                    Nothing
                        )
            )
        |> List.concat
        |> List.filterMap
            (Maybe.map
                (\( x, y, direction ) ->
                    WorkingCart
                        { position = ( x, y )
                        , direction = direction
                        , nextIntersection = TurnLeft
                        }
                )
            )


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


getCrashLocation : Cart -> Maybe ( Int, Int )
getCrashLocation cart =
    case cart of
        WorkingCart _ ->
            Nothing

        CrashedCart crash ->
            Just crash


stepTurn : Turn -> Turn
stepTurn turn =
    case turn of
        TurnLeft ->
            TurnStraight

        TurnStraight ->
            TurnRight

        TurnRight ->
            TurnLeft


positionToString : ( Int, Int ) -> String
positionToString ( x, y ) =
    String.fromInt x ++ "," ++ String.fromInt y


findCrash : ( Tracks, Carts ) -> ( Int, Int )
findCrash ( tracks, carts ) =
    carts
        |> List.filterMap getCrashLocation
        |> List.head
        |> Advent.unsafeMaybe


findAlive : ( Tracks, Carts ) -> ( Int, Int )
findAlive ( _, carts ) =
    carts
        |> List.filterMap
            (\c ->
                if hasCrashed c then
                    Nothing

                else
                    Just (cartPosition c)
            )
        |> List.head
        |> Advent.unsafeMaybe


trackToChar : Track -> Char
trackToChar track =
    case track of
        Vertical ->
            '|'

        Horizontal ->
            '-'

        DiagonalToRight ->
            '/'

        DiagonalToLeft ->
            '\\'

        Intersection ->
            '+'


show : String -> Input1 -> Input1
show message ( tracks, carts ) =
    let
        cartPositions : Dict ( Int, Int ) Char
        cartPositions =
            carts
                |> List.map
                    (\c ->
                        case c of
                            WorkingCart { position, direction } ->
                                ( position
                                , case direction of
                                    Right ->
                                        '>'

                                    Left ->
                                        '<'

                                    Up ->
                                        '^'

                                    Down ->
                                        'v'
                                )

                            CrashedCart position ->
                                ( position, 'X' )
                    )
                |> Dict.fromList

        trackCoords : List ( Int, Int )
        trackCoords =
            Dict.keys tracks

        maxX : Int
        maxX =
            trackCoords
                |> List.map Tuple.first
                |> List.maximum
                |> Advent.unsafeMaybe

        maxY : Int
        maxY =
            trackCoords
                |> List.map Tuple.second
                |> List.maximum
                |> Advent.unsafeMaybe

        output : String
        output =
            List.range 0 maxY
                |> List.map
                    (\y ->
                        List.range 0 maxX
                            |> List.map
                                (\x ->
                                    let
                                        track : Maybe Char
                                        track =
                                            Dict.get ( x, y ) tracks
                                                |> Maybe.map trackToChar

                                        cart : Maybe Char
                                        cart =
                                            Dict.get ( x, y ) cartPositions
                                    in
                                    cart
                                        |> Maybe.withDefault (track |> Maybe.withDefault ' ')
                                )
                            |> String.fromList
                    )
                |> String.join "\n"

        _ =
            Debug.log (output ++ "\n") message
    in
    ( tracks, carts )


cartPosition : Cart -> ( Int, Int )
cartPosition cart =
    case cart of
        WorkingCart { position } ->
            position

        CrashedCart position ->
            position


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


step : StopBehaviour -> ( Tracks, Carts ) -> ( Tracks, Carts )
step stopBehaviour ( tracks, carts ) =
    let
        sortedCarts : List Cart
        sortedCarts =
            carts
                |> List.sortBy (swap << cartPosition)

        newCarts =
            sortedCarts
                |> List.foldl (stepCart stopBehaviour tracks) ( carts, [] )
                |> Tuple.first
    in
    ( tracks, newCarts )


stepCart : StopBehaviour -> Tracks -> Cart -> ( Carts, List ( Int, Int ) ) -> ( Carts, List ( Int, Int ) )
stepCart stopBehaviour tracks cart ( allCarts, crashedPositions ) =
    --let
    --    _ =
    --        Debug.log "processing" (cartPosition cart)
    --in
    case cart of
        WorkingCart c ->
            if List.member c.position crashedPositions then
                ( allCarts
                , crashedPositions
                )

            else
                let
                    newPosition : ( Int, Int )
                    newPosition =
                        stepInDirection c.direction c.position

                    newTrack : Track
                    newTrack =
                        getTrack tracks newPosition

                    newlyOnIntersection : Bool
                    newlyOnIntersection =
                        newTrack == Intersection

                    newDirection : Direction
                    newDirection =
                        stepDirection newTrack c.nextIntersection c.direction

                    newNextIntersection : Turn
                    newNextIntersection =
                        if newlyOnIntersection then
                            stepTurn c.nextIntersection

                        else
                            c.nextIntersection

                    newUncrashed : Cart
                    newUncrashed =
                        WorkingCart
                            { position = newPosition
                            , direction = newDirection
                            , nextIntersection = newNextIntersection
                            }

                    newCrashed : Cart
                    newCrashed =
                        CrashedCart newPosition

                    withoutOld : Carts
                    withoutOld =
                        List.filter ((/=) cart) allCarts

                    crashed : Bool
                    crashed =
                        List.any (\cc -> cartPosition cc == newPosition) allCarts
                in
                if crashed then
                    --let
                    --    _ =
                    --        Debug.log "crashed!" ()
                    --in
                    case stopBehaviour of
                        StopAtFirstCrash ->
                            ( newCrashed
                                :: (withoutOld |> List.filter (\cc -> cartPosition cc /= newPosition))
                            , newPosition :: crashedPositions
                            )

                        StopAtLastAlive ->
                            ( withoutOld
                                |> List.filter (\cc -> cartPosition cc /= newPosition)
                            , newPosition :: crashedPositions
                            )

                else
                    ( newUncrashed :: withoutOld
                    , crashedPositions
                    )

        CrashedCart _ ->
            ( allCarts, crashedPositions )


stepDirection : Track -> Turn -> Direction -> Direction
stepDirection track nextIntersection oldDirection =
    case track of
        Intersection ->
            case nextIntersection of
                TurnLeft ->
                    left oldDirection

                TurnStraight ->
                    oldDirection

                TurnRight ->
                    right oldDirection

        Vertical ->
            oldDirection

        Horizontal ->
            oldDirection

        DiagonalToRight ->
            case oldDirection of
                Up ->
                    Right

                Left ->
                    Down

                Right ->
                    Up

                Down ->
                    Left

        DiagonalToLeft ->
            case oldDirection of
                Down ->
                    Right

                Left ->
                    Up

                Right ->
                    Down

                Up ->
                    Left


left : Direction -> Direction
left direction =
    case direction of
        Left ->
            Down

        Up ->
            Left

        Right ->
            Up

        Down ->
            Right


right : Direction -> Direction
right direction =
    case direction of
        Left ->
            Up

        Up ->
            Right

        Right ->
            Down

        Down ->
            Left


getTrack : Tracks -> ( Int, Int ) -> Track
getTrack tracks position =
    Dict.get position tracks
        |> Advent.unsafeMaybe


stepInDirection : Direction -> ( Int, Int ) -> ( Int, Int )
stepInDirection direction ( x, y ) =
    case direction of
        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


progress : StopBehaviour -> ( Tracks, Carts ) -> ( Tracks, Carts )
progress stopBehaviour ( tracks, carts ) =
    case stopBehaviour of
        StopAtFirstCrash ->
            if hasAnyCrashed carts then
                ( tracks, carts )

            else
                progress stopBehaviour (step stopBehaviour ( tracks, carts ))

        StopAtLastAlive ->
            if hasMoreThanOneAlive carts then
                progress stopBehaviour (step stopBehaviour ( tracks, carts ))

            else
                ( tracks, carts )


hasMoreThanOneAlive : Carts -> Bool
hasMoreThanOneAlive carts =
    List.length (List.filter (not << hasCrashed) carts) > 1


hasAnyCrashed : Carts -> Bool
hasAnyCrashed carts =
    List.any hasCrashed carts


hasCrashed : Cart -> Bool
hasCrashed cart =
    case cart of
        WorkingCart _ ->
            False

        CrashedCart _ ->
            True


type StopBehaviour
    = StopAtFirstCrash
    | StopAtLastAlive


compute1 : Input1 -> Output1
compute1 ( tracks, carts ) =
    ( tracks, carts )
        |> progress StopAtFirstCrash
        |> show "after crash"
        |> findCrash
        |> positionToString


compute2 : Input2 -> Output2
compute2 ( tracks, carts ) =
    -- TODO: doesn't work yet
    ( tracks, carts )
        |> progress StopAtLastAlive
        |> show "last alive"
        |> findAlive
        |> positionToString



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    []


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


blabla : String
blabla =
    """
/>>-\\
|   |
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/
       """
        |> Advent.removeNewlinesAtEnds


input_ : String
input_ =
    """
                          /------------------------------\\                                           /----------\\
                          |             /----------------+----------------------------------------\\  |          |
                          |             |                |                                    /---+--+----------+-------------------------------\\
                          |             |        /-------+------------------------------------+---+--+----------+-----------------------------\\ |
                          |             |        | /-----+-----------------------------\\/-----+---+--+----------+--------\\                    | |
    /-------------->\\     |             |        | |     |                             ||     |   |  |   /------+--------+--------------------+\\|
    |               |     |             |   /----+-+-----+---------------\\             ||     |   |  |   |      |        |                    |||
    |    /----------+-----+\\  /---------+---+----+-+-----+---------------+---------\\   ||/----+---+\\ |   |      |        |                    |||
/---+----+----------+-----++--+----\\    |   |    | |     |           /---+----\\ /--+---+++----+---++-+---+-->->-+--------+--\\                 |||
|   |    |          |    /++--+----+----+---+----+-+-----+-----------+---+----+-+--+---+++----+---++-+---+------+--\\     |  |                 |||
|   |/---+----------+----+++--+----+--\\ |   |    | |     |           |   |    | |  |   |||    |   || \\---+------/  |     |  |                 |||
|   ||   |          |    |||  |    |  |/+---+----+-+-----+-----------+--\\|    | |  |   |||    |   ||     |         |     |  |          /------+++----\\
|   ||   |     /----+----+++--+----+--+++---+----+-+-----+-----------+--++----+-+--+---+++----+---++-----+-------\\ |     |  |          |      |||    |
|   ||   |/----+----+----+++--+----+--+++---+----+-+-----+-----------+--++-\\  | |  |   |||/---+---++-----+--\\    | |     | /+---------\\|      |||    |
|   ||   ||    |    |    |||  |  /-+--+++---+----+-+-\\   |/----------+--++-+--+-+--+---++++---+---++-----+--+----+-+-----+-++\\        ||      |||    |
|   ||   ||  /-+----+----+++--+--+-+--+++---+----+-+-+---++----\\  /--+--++-+--+-+--+---++++---+<--++-----+--+----+-+-----+-+++-------\\||      |||    |
|   ||   ||  | |    |    ||| /+--+-+--+++---+----+-+-+---++----+--+--+--++-+--+-+--+---++++---+---++-----+--+----+-+-<---+-+++---\\ /-+++------+++\\   |
|   ||   ||  | |    |    ||| ||  | |  |||   |    | | |   ||    |  | /+--++-+--+-+--+---++++---+\\  ||     \\--+----+-+-----+-+++---+-+-+++------+/||   |
|   ||   ||  | |    |    ||| ||  | |  |||   |    | | |   ||    |  | ||  || |  |/+--+---++++---++--++---\\    |    | |     | |||   | | |||      | ||   |
|   ||/--++--+-+----+----+++-++--+-+--+++---+----+-+-+---++----+--+-++--++-+--+++--+\\  ||||   ||  ||   |    |    | |     | |||   | | |||      | ||   |
|   |||  ||  | |    |    ||| ||  | | /+++---+----+-+-+---++----+--+-++--++-+--+++--++--++++---++--++---+----+----+-+---\\ | |||   | | |||      | ||   |
|   |||  ||  | |    |    ||| ||  | | ||||   |    | | |   ||    |  | ||  || |  |||  || /++++---++--++---+----+----+-+---+-+-+++---+-+-+++----\\ | ||   |
|   |||  ||  | |    |    ||| ||  | | |||\\---+----+-+-+-->++----+--+-++--++-+--+++--++-+++++---++--/|   |    |    | |   | | |||   | | |||    | | ||   |
|   |||  ||  | |    |    ||| ||  | | |||    |    | | |   ||    |  | || /++-+--+++--++-+++++---++---+---+----+----+-+---+-+-+++---+\\| |||    | | ||   |
|   |||  ||  \\-+----+----+++-++--+-+-+++----+----+-+-+---++----/  | || ||| |  |||  || |||||   ||   |   |    |    | |   | | |||   ||| |||    | | ||   |
|   |||  || /--+----+----+++-++--+-+-+++----+--\\ | | |   ||/------+-++-+++-+--+++--++-+++++---++---+---+----+----+-+-\\ | | |||   ||| |||    | | ||   |
|  /+++--++-+--+----+----+++-++--+-+-+++----+--+-+-+-+---+++------+-++-+++-+--+++--++\\|||||   ||   |   |    |    | | | | | |||   ||| |||    | | ||   |
|  ||||  || | /+----+----+++-++--+-+-+++----+--+-+-+-+---+++------+-++-+++-+--+++--++++++++---++---+---+----+----+-+-+-+-+\\|||   ||| |||    | | ||   |
|  ||\\+--++-+-++----+----+++-++--+-+-+/|    |  | | | |   |||      |/++-+++-+--+++--++++++++---++---+---+----+\\   | | | | |||||   ||| |||    | | ||   |
|  || |  || | || /--+--<-+++-++--+-+-+-+----+--+-+-+-+---+++------++++-+++-+--+++--++++++++---++---+--\\|    ||   | | | | |||||   ||| |||    | | ||   |
| /++-+--++-+-++-+--+----+++-++--+-+-+-+----+--+-+-+\\|   |||    /-++++-+++-+--+++--++++++++---++---+--++---\\||   | | | | |||||   ||| |||    | | ||   |
| ||| |  || | || |  |    ||| ||  | | | |    |  | | |||   |||/---+-++++-+++-+--+++--++++++++---++-\\ |  ||   |||   | | | | |||||   ||| |||    | | ||   |
| ||| |  || |/++-+--+----+++-++--+-+-+-+----+--+-+-+++---++++---+-++++-+++-+--+++\\ |||||||\\---++-+-+--++---+/|   | | | | |||||   ||| |||    | | ||   |
| ||| |  || |||| |  |    ||| ||  | | |/+----+--+-+-+++---++++---+-++++-+++-+--++++-+++++++----++-+-+--++---+-+---+-+-+-+-+++++\\  ||| |||    | | ||   |
| ||| |  || |||| |  |    ||| ||  | | |||    |  | | |||   ||||   | |||| ||| |  |||| |||||||    || | |  ||   | |   | | | | ||||||  ||| |||    | | ||   |
| ||| |  || |||| |  |    |\\+>++--+-+-+++----+--+-+-+++---/|||   | |||| ||| |  |||| |||||||    || | |  ||   | |   | | | | ||||||  ||| |||    | | ||   |
| ||| |  || |||| |  |   /+-+-++--+-+-+++----+--+-+-+++--\\ |||   | |||| ||| |  |||| |||||||    || | |/-++---+-+---+-+-+-+-++++++--+++-+++---\\| | ||   |
| ||| |  || ||||/+--+---++-+-++--+\\| ||\\----+--+-+-+++--+-+++---+-++++-+/| |  |||| |||||||    || | || ||   | |   | | | | ||||||  ||| |||   || | ||   |
| ||| |  \\+-++++++->+---++-/ ||  \\++-++-----+--+-+-++/  | |||   | |||| | | |  |||| |||||||    || |/++-++---+-+---+-+-+-+-++++++--+++-+++\\  || | ||   |
| ||| |   | ||||||/-+---++---++---++-++-----+--+-+-++---+-+++---+-++++-+-+-+-\\|||| |||||||    || |||| ||   | |   | | | | ||||||  ||| ||||  || | ||   |
| ||| |   | ||||||| |   ||   ||   || || /---+--+-+-++---+-+++---+-++++-+-+-+-+++++-+++++++----++-++++-++---+-+\\  | | | | ||||||  ||| ||||  || | ||   |
| ||| |   | ||||||| |   ||   ||   || || |   |  | | ||   |/+++---+-++++-+-+-+\\||||| |||||||    || |||| ||   | ||  | | | | ||||||  ||\\-++++--++-+-+/   |
| ||| |   | ||||||| |   ||   ||   || || |   |  | | ||   |||||/--+-++++-+-+-+++++++-+++++++----++-++++-++---+-++--+-+-+-+-++++++--++\\ ||||  || | |    |
| ||| |   | ||||||| |   ||   ||   || || |   |  | | ||  /++++++--+-++++-+-+-+++++++-+++++++----++-++++-++---+-++--+-+-+-+-++++++--+++-++++\\ || | |    |
| ||| |   | ||||||| |   ||   \\+---++-++-+---+--+-+-++--+++++++--+-++++-+-+-+++++++-+++++++----++-++++-++---+-++--+-+-+-+-++++++--/|| ||||| || | |    |
| ||| |   | ||||||| |   ||  /-+---++-++-+---+--+-+-++--+++++++--+-++++-+-+-+++++++-+++++++----++-++++-++---+-++--+-+-+\\| ||||||   || ||||| || | |    |
| ||| |   | ||||||| |   ||  | |   || || |   |  | | ||  |||||||  | |||| | | ||||||| |||||||    || |||| ||   | ||  | | ||| ||||||   || ||||| || | |    |
| ||| |   | ||||||| |   ||  | |   || || |   |  | | ||  |||||||  | |||| | | ||||||| |||||||    || ||||/++---+-++--+-+-+++>++++++---++-+++++-++-+\\|    |
\\-+++-+---+-+++++++-+---++--+-+---+/ || |   |  | | \\+--+++++++--+-++++-+-+-+++++++-++++/||    || |||||||   | ||  | | ||| ||||||   || ||^|| || |||    |
  ||| |   | ||||||| |   || /+-+---+--++-+---+--+-+--+--+++++++-\\| |||| | | ||||||| |||| ||    || |||||||   | ||  | | ||| ||\\+++---++-+/||| || |||    |
  ||| |   | ||||||| |   || || |   |  || |   |  | |  |  ||||||| || |||\\-+-+-+++/||| |||| ||    || |||||||   | ||  | | ||| || |||   || | ||| || |||    |
  ||| |   | ||||||| |   || || |   |  || |   |  | |  |  ||||||| || |||  | | ||| ||| |||| ||/---++-+++++++---+-++-\\| | ||| || |||   || | ||| || |||    |
  ||| |   | ||||||| |   ||/++-+---+--++-+---+--+-+--+--+++++++-++-+++--+-+-+++-+++-++++-+++---++-+++++++---+-++-++-+-+++-++-+++\\  || | ||| || |||    |
  ||| |   | ||||||| |   |\\+++-+---+--++-+---+--+-+--+--+++++++-++-+++--+-+-+++-+++-++++-+++---++-+++++++---+-++-++-/ ||| || ||||  || | ||| || |||    |
  \\++-+---+-+++++++-+---+-+++-+---+--++-+---+--+-+--/  ||||\\++-++-+++--+-+-+++-+++-++++-+++---++-+++++++---+-++-++---/|| || ||||  || |/+++-++-+++---\\|
   || |   | ||||||| |   | ||| | /-+--++-+---+--+-+-----++++-++-++-+++--+-+-+++-+++-++++-+++---++-+++++++---+-++-++----++-++-++++--++-+++++\\|| |||   ||
   || |   \\-+++++++-+---+-+++-+-+-+--++-+---+--+-+-----++++-++-++-+++--+-+-/|| ||| |||| \\++---++-+++++++---+-++-++----++-/| ||||  || |||||||| |||   ||
   || |     ||||||| | /-+-+++-+-+-+--++-+-\\ |/-+-+-----++++-++\\|| |||  | |  || \\++-++++--++---++-++++++/   | || ||    ||  | ||||  || |||||||| |||   ||
   || |     |||||\\+-+-+-+-+++-+-+-+--++-+-+-++-+-+-----++++-+++++-+++--+-+--++--++-++++--++---++-+++++/   /+-++-++-\\  ||  | ||||  || |||||||| |||   ||
   || |     ||||| | | | | ||| | | |  || | | || | |     |||| ||||| |||  | |/-++--++-++++--++---++-+++++----++-++-++-+--++--+-++++--++-++++++++\\|||   ||
  /++-+-----+++++-+-+-+-+-+++-+-+-+--++-+-+-++-+-+-----++++-+++++-+++--+-++-++--++-++++--++---++-+++++--\\ || || || |  ||  | ||||  || ||||||||||||   ||
  ||| |     ||||| | | | | ||| | | |  || | | || | |     |||| ||||| |||  | || ||  || ||||  ||   || |||||  | || || || |  ||  | ||||  || ||||||||||||   ||
  ||| |    /+++++-+-+-+-+-+++-+-+-+--++-+-+-++-+-+-----++++-+++++-+++--+-++-++\\ || ||||  ||   || |||||  | || || || |  ||  | ||||  || ||||||||||||   ||
  ||\\-+----++++++-+-/ | | ||| | | |  |\\-+-+-++-+-+-----++++-+++++-+++--+-++-+++-++-++++--++---++-+++++--+-++-++-++-+--++--+-++/|  || ||||||||||||   ||
  ||  |    |||||| |   | | ||| | | |  |  |/+-++-+-+----\\|||| ||||| ||\\--+-++-+++-++-++++--++---+/ ||||\\--+-++-++-++-+--++--+-++-+--++-++++++++++/|   ||
  ||/-+----++++++-+---+-+-+++-+-+-+--+--+++-++\\| |    |||||/+++++-++---+-++-+++-++-++++--++---+--++++---+-++-++-++-+--++--+-++-+--++\\|||||||||| |   ||
  ||| |    |||||| |   | | ||| | | |  |  ||| |||| |    ||||||||||| ||   | || ||| || ||||  ||   |  ||||   | || || || |  ||  | || |  ||||||||||||| |   ||
  ||| |    |||||| |   | | |||/+-+-+--+--+++-++++-+----+++++++++++-++---+-++-+++-++-++++--++---+--++++---+-++-++-++-+\\ ||  | || |  ||||||||||||| |   ||
  ||| | /--++++++-+---+-+-+++++-+-+--+--+++-++++-+----+++++++++++-++---+-++-+++-++-++++--++---+--++++---+-++\\|| || || ||  | || |  ||||||||||||| |   ||
  ||| | |/-++++++-+---+-+-+++++-+-+--+-\\||| |||| |    ||||||||||| ||   | || ||| || ||||  ||   |  ||||   | ||||| || || ||  | || |  ||||||||||||| |   ||
  ||| | || |||||| |   | | ||||| \\-+--+-++++-++++-+----+++++++++++-++---+-++-+++-++-++++--++---+--++++---+-+++++-++-++-++--+-++-+--++++++++/|||| |   ||
  ||| | || |||||| |   | | ||||\\---+--+-++++-++++-+----+++++++++++-++---+-++-+++-++-/|||  ||   |  ||||   | ||||| || || ||  | || |  |||||||| |||| |   ||
/-+++-+-++-++++++-+---+-+-++++----+--+-++++-++++-+-\\/-+++++++++++-++---+-++-+++-++--+++--++---+\\ ||||   | ||||| || || ||  | || |  |||||||| |||| |   ||
| ||| | || |||||| |   | | ||||    |  | |||| |||| | || ||||||\\++++-++---+-++-+++-++--+++--++---++-/|||   | ||||| || || ||  | || |  |||||||| |||| |   ||
| ||| | || |||||| |   | | ||||    |  | |||| |||| | || |||||| |||| ||  /+-++-+++-++--+++--++---++--+++---+-+++++-++-++-++--+-++-+--++++++++-++++-+\\  ||
| ||| | || |||||| |   | | ||||    |  | |||| |||| \\-++-++++++-++++-++--++-++-+++-++--+++--++---++--+++---+-+++++-++-++-++--+-++-+--++++++++-+++/ ||  ||
| ||| | || |||||| |   | |/++++----+--+-++++-++++---++-++++++-++++-++--++-++-+++-++--+++--++-\\ ||  |||   | ||||| || || ||  | || |  |||||||| |||  ||  ||
| ||| | || |||||| |   | ||||||    |  | |||| ||||   || |||||| |||| ||  || || ||| ||  |||  || | ||  |||   | ||||| || || ||  | || |  |||||||| |||  ||  ||
| ||| | ||/++++++-+--\\| ||||||    |  | |||| ||||   || |||||| |||| ||  || || ||| ||  |||  \\+-+-++--+/\\---+-+++++-++-++-++--+-++-+--++++++++-/||  ||  ||
|/+++-+-+++++++++-+--++-++++++\\   |  | |||| ||||   || |||||| |||| ||  |\\-++-+++-++--+++---+-+-++--+-----+-+++++-++-++-++--+-++-+--/|||||||  ||  ||  ||
||||| | ||||||||| |  || |||||||   |  | |||| ||||   || |||||| |||| ||  |  || ||| ||  |||   | | ||  |     | ||||| || || ||  | || |   |||||||  ||  ||  ||
||||| | ||||||||| |  || |||||||   |  | |||| ||||   || |||||| |||| ||  |  || ||| ||  |||   | | ||  |     | ||||| || || ||  | || |   |||||||  ||  ||  ||
||||| | ||||||||| |  || |||||||   |  | |||| ||||   || |||||| |||\\-++--+--++-+++-++--+++---+-+-++--+-----+-+/||| || || ||  | || |   |||\\+++--++--++--/|
||||| | ||||||||| |  || |||||||   |  | |||| ||||   || |||||| |||  ||  |  || ||| ||  |||   | | ||  |     | | ||| || || ||  | || |   ||| |||  ||  ||   |
||||\\-+-+++++++++-+--++-+++++++---+--+-++++-++/|/--++-++++++-+++--++--+--++-+++-++--+++---+-+-++--+-----+-+-+++-++-++-++\\ | || |   ||| |||  ||  ||   |
||||  | ||||||||| |  || |||||||   |  | |||| || ||  || |||||| |||  ||  |  || ||| \\+--+++---+-+-++--+-----+-+-+++-++-++-+++-+-/| |   ||| |||  ||  ||   |
||||  | ||||||||| |  || |||||||/--+--+-++++-++-++--++-++++++-+++--++--+--++-+++--+--+++---+-+-++--+-----+-+-+++\\|| || ||| |  | |   ||| |||  ||  ||   |
||||  | ||||||||| |  ||/++++++++--+--+-++++-++-++--++-++++++-+++--++--+--++-+++--+--+++---+-+-++--+-----+\\| |||||| || ||| |  | |   ||| |||  ||  ||   |
||||  | |||\\+++++-+--+++++++++++--+--+-++++-++-++--++-++++++-+++--++--+--++-++/  |  |||   | | ||  |     ||| |||||| || ||| |  | |   ||| |||  ||  ||   |
||||  | ||| ||||| |  |||\\+++++++--+--+-++++-++-++--++-++/||\\-+++--++--+--++-++---+--+++---+-+-++--+-----+++-++++++-++-+++-+--+-+---+/| |||  ||  ||   |
||||  |/+++-+++++-+--+++-+++++++--+--+-++++-++-++--++-++-++--+++--++--+--++-++---+--+++---+-+-++--+---\\ ||| |||||| || ||| |  | |   | | |||  ||  ||   |
||||  ||||| ||||| |  ||| |||||||  |  | |||| || ||  || ||/++--+++--++--+--++-++---+--+++---+-+-++--+---+-+++-++++++-++-+++-+--+-+---+-+-+++--++--++-\\ |
||||  ||||| \\++++-+--+++-+++++++--+--+-++++-++-/|  || |||||  |||  ||  |  || ||   |  |||   | | \\+--+---+-+++-++++++-++-+++-+--+-+---+-+-+++--++--/| | |
||||  |||||  |||| |  ||| |||||||  |  | |||| ||  |/-++-+++++--+++--++--+--++-++---+--+++---+-+--+--+---+-+++-++++++\\|| ||| |  | |   | | |||  ||   | | |
||||  |||||  |||| |  ||| |||||||  |  | |||| ||  || || |||||  |||  ||  \\--++-++---+--+++---+-+--+--+---+-+++-+++++++++-+++-+--+-+---+-+-+++--++---/ | |
||||  |||||  |||| |  ||| |||||||  |  | |||| ||  || || |||||  |||  |\\-----++-++---+--+++---+-+--+--+---+-+++-+/||||||| ||| |  | |   | | |||  ||     | |
||||  |||||/-++++-+--+++-+++++++--+--+-++++-++--++-++-+++++--+++--+------++-++--\\|  |||   | |  |  |   | ||| | ||||||| ||| |  | |   | | \\++--++-----+-/
||||  |||||| |||| |  ||| |||||||  |  | |||| ||  || || |||||  |||  |      || ||  ||  |||   | |  |  |   | ||| | ||||||| ||| |  | |   | |  ||  ||     |
||||  |||||| |||| | /+++-+++++++--+--+-++++-++--++-++-+++++--+++--+------++-++--++--+++---+-+--+--+--\\| ||| | ||||||| ||| |  | |   | |  ||  ||     |
||||  |||||| |||| | |||| |||||||  |  | |||| ||  || || |||||  |||  |      || ||  ||  |||   | |  |  |  || ||| | ||||||| ||| |  | |   | |  ||  ||     |
||||  |||||| |||| | |||| |||||||  |  | |||| ||  || || |||||  |||  \\------++-++--++--+++---+-+--+--+--++-+++-+-+++++++-+++-+--+-+---+-/  ||  ||     |
||||  |||||| |||| | |||| ||\\++++--+--+-++++-++--++-++-+++++--++/         || ||  ||  |||   |/+--+--+--++-+++\\| ||||||| ||| |  |/+---+---\\||  ||     |
||||  |||||| |||| | |||| || ||||  |  | |||| \\+--++-++-+++++--++----------/| ||  ||  |||   |||  |  |  || ||||| ||||||| ||| |  |||   |   |||  ||     |
||||  ||||\\+-++++-+-+/|| |^ ||||  |  | ||||/-+--++-++-+++++--++-----------+-++--++--+++---+++--+--+--++-+++++-+++++++-+++-+--+++---+---+++--++---\\ |
||||  |||| | ||||/+-+-++-++-++++-\\|  | ||||| |  || || ||||\\--++-----------+-++--++--+++---+++--+--+--++-+++++-+++++++-+++-+--/||   |   |||  ||   | |
||||  |||| | ||\\+++-+-++-++-++++-++--+-+++++-+--++-++-++++---++-----------+-++--++--+++---+++--+--+--++-+++++-+++/||| ||| |   ||   |   |||  ||   | |
||||  ||\\+-+-++-+++-+-++-++-++++-++--+-+++++-+--++-++-++++---++-----------+-++--++--+++---+++--+--+--++-++++/ ||| ||| ||| |   ||   |   |||  ||   | |
||||  || | | || ||| | || || |||| ||  | ||||| |  || || ||||   ||           | ||  ||  ||\\---+++--+--+--++-++++--+++-+++-+++-+---++---+---+++--/|   | |
||||  || | | || ||| | || |\\-++++-++--+-+++++-+--++-++-++++---++-----------+-++--++--++----+++--+--+--++-++++--+++-+++-+++-+---+/   |   |||   |   | |
||||  || | | || ||| | || |  |||| ||  | ||||| |  || || ||||   ||/----------+-++--++--++----+++--+--+--++-++++--+++-+++-+++-+---+----+---+++---+---+-+-\\
||||  || | | || ||| | || |  |||| ||  | |\\+++-+--++-++-++++---+++----------+-++--++--++----+++--+--+--++-++++--/|| ||| ||| |   |    |   |||   |   | | |
||||  || | | || ||| \\-++-+--++++-++--+-+-+++-+--++-++-++++---+++----------+-++--++--++----+++--+--+--/| ||||   || ||| ||| |   |    |   |||   |   | | |
||||  \\+-+-+-++-+++---++-+--++++-++--+-+-+++-+--++-++-++++---+++----------+-++--++--/|    |||  |  |   | ||||   || ||| ||| |   |    |   |||   |   | | |
||||   | | | || |||   || |  |\\++-++--+-+-+++-+--++-++-++++---+++----------+-++--++---+----+++--+--+---+-++++---++-++/ ||| |   |    |   |||   |   | | |
||||   | | | || |\\+---++-+--+-++-/|  \\-+-+++-+--++-++-++++---+++----------+-++--++---+----+++--+--+---+-++++---++-++--+/| |   |    |   |||   |   | | |
\\+++---+-+-+-++-+-+---++-+--+-++--+----+-+++-+--++-/| |||\\---+++----------+-/|  ||   |    \\++--+--+---+-++++---+/ ||  | | |   |    |   |||   |   | | |
 |||/--+-+-+-++-+-+---++-+->+-++--+-\\  | ||| |  ||  | |||    \\++----------+--+--++---+-----++--+--+---+-++++---+--++--+-+-+---+----/   |||   |   | | |
 ||||  | | | || | |   || |  | ||  | |  | ||| |  ||  | |^|     ||          |  |  ||   |     \\+--+--+---+-+++/   |  ||  | | |   |        |||   |   | | |
 ||||  | | | |\\-+-+---++-+--+-++--+-+--+-+++-+--++--+-+++-----++----------+--+--++---+------+--+--+---+-+++----+--++--+-+-/   |        |||   |   | | |
 ||||  | | | |  | |   || |  | ||  | |  | ||| |  ||  | |||     ||         /+--+--++---+------+--+--+---+-+++----+--++--+-+-----+-\\      |||   |   | | |
 ||||  | | | |  | \\---++-+--+-++--+-+--+-+++-+--++--+-+++-----++---------++--/  ||   |      |  |  |   | |||    |  ||  | |     | |    /-+++---+\\  | | |
 ||\\+--+-+-+-+--+-----++-+--+-++--+-+--+-+++-+--++--+-+++-----++---------++-----++---/      |  |  |   | |||    |  ||  | |     | |    | |||   ||  | | |
 || |  | | | |  |     || |/-+-++--+\\|  | ||| |  ||/-+-+++--\\  ||         ||     ||          |  |  |   | |||    |  ||  | |     | |    | |||   ||  | | |
 || |  | | | |  |     || || | ||  |||  | |||/+--+++-+\\|||  |  ||         ||     ||          |  |  |   | |||    |  ||  | |     | |    | |||   ||  | | |
 || |  | | | \\--+-----++-++-+-++--+++--+-+++++--+++-+++++--+--++---------++-----+/          |  |  |   | |||    |  ||  | |     \\-+----+-/||   ||  | | |
 || |  | | |    |     || || | ||  |||  | |||||  ||| |||||  |  ||         ||     |           |  |  |   | |||    |  ||  | |       |    |  ||   ||  | | |
 || |  | | |    |     \\+-++-+-++--+++--+-+/|||  ||| |||||  |  ||         ||     |           |  |  |   | |||    |  ||  | |       |    |  ||   ||  | | |
 || |  | | |    |      | || | ||  |||  | | |||  ||| \\++++--+--++---------++-----+-----------+--/  |   | |||    |  ||  | |       |    |  ||   ||  | | |
 || |  \\-+-+----+------+-++-+-++--+++--+-+-+++--+++--++++--+--++---------++-----+-----------+-----+---/ |||    |  ||  | |       |    |  ||   ||  | | |
 || |    | |    |      | || | ||  |||  | \\-+++--+++--+/|| /+--++---------++-----+-----------+-----+----\\|||    |  ||  | |       |    |  ||   ||  | | |
 || |    | |    |      | || | ||  ||v  |   |||  |||  | || ||  ||         ||     |           |     |    ||||    |  ||  | |       |    |  ||   ||  | | |
 || |    | \\----+------+-++-+-++--+++--+---+++--+++--+-++-++--++---------++-----/           |     |    ||||    |  ||  | |       |    |  ||   ||  | | |
 || |    |      ^      | || | ||  |||  |   |||  |||  | |\\-++--++---------++-----------------+-----+----++++----+--++--+-+-------+----+--++---++--+-/ |
 || |    |      \\------+-++-+-++--/||  |   |||  \\++--+-+--++--++---------++-----------------+-----+----++++----+--++--+-/       |    |  ||   ||  |   |
 || |    |             | || | ||   ||  |   |||   ||  | |  ||  ||         ||                 |     |    ||||    |  ||  |         |    |  ||   ||  |   |
 || |    |             | \\+-+-++---++--+---+++---++--+-+--++--++---------++-----------------/     |    ||||    |  ||  |         |    |  ||   ||  |   |
 || |    |             |  | \\-++---++--+---+++---++--+-+--++--++---------++-----------------------+----++++----+--++--/         |    |  ||   ||  |   |
 \\+-+----+-------------+--+---/|   ||  |   |||   ||  | |  ||  ||         ||                       \\----++++----+--++------------+----+--/|   ||  |   |
  | |    |             |  |    \\---++--+---+++---++--+-+--++--++---------++----------------------------++++----/  ||            |    |   |   ||  |   |
  \\-+----+-------------+--+--------++--+---+++---++--+-+--++--++---------++----------------------------+/|\\-------+/            |    |   |   ||  |   |
    |    |             |  |        ||  |   |||   ||  | |  \\+--++---------++--------------------<-------/ |        |             |    |   |   ||  |   |
    |    |             |  |        ||  |   |||   \\+--+-+---+--++---------++------------------------------+--------/             |    |   |   ||  |   |
    |    |             |  |        ||  |   |||    \\--+-+---/  ||         ||                              |                      |    |   |   ||  |   |
    |    |             \\--+--------++--+---+++-------+-+------++---------++------------------------------/                      |    |   |   ||  |   |
    \\----+----------------+--------+/  |   |||       | |      ||         \\+-----------------------------------------------------/    \\---+---+/  |   |
         |                |        |   |   \\++-------+-+------++----------+--------------------------------------------------------------+---+---/   |
         |                |        |   |    ||       | |      |\\----------+--------------------------------------------------------------+---+-------/
         |                \\--------/   |    |\\-------+-+------/           |                                                              |   |
         |                             |    |        | \\------------------+--------------------------------------------------------------/   |
         \\-----------------------------/    \\--------/                    \\------------------------------------------------------------------/
"""
        |> Advent.removeNewlinesAtEnds


main : Program () ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input_
        , parse1 = parse1
        , parse2 = parse2
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }