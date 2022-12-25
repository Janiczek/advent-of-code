import Day24.Companion.Dir.*
import Day24.Companion.Dir
import kotlin.math.abs

class Day24 {
    companion object {
        const val inputFile = "input202224.txt"

        enum class Dir {N,S,W,E}
        data class Input(
            val blizzards: List<Pair<XY,Dir>>,
            val walls: Set<XY>,
            val party: XY,
            val goal: XY,
        )

        fun parse1(input: String): Input {
            val blizzards = input.lines().mapIndexed { y,line ->
                line.mapIndexedNotNull { x,char ->
                    when (char) {
                        '>' -> (x.toLong() to y.toLong()) to E
                        '<' -> (x.toLong() to y.toLong()) to W
                        '^' -> (x.toLong() to y.toLong()) to N
                        'v' -> (x.toLong() to y.toLong()) to S
                        else -> null
                    }
                }
            }.flatten()
            val walls = input.lines().mapIndexed { y,line ->
                line.mapIndexedNotNull { x,char ->
                    when (char) {
                        '#' -> x.toLong() to y.toLong()
                        else -> null
                    }
                }
            }.flatten().toSet()
            val party = input.lines().first().indexOf('.').toLong() to 0L
            val goal = input.lines().last().indexOf('.').toLong() to (input.lines().size - 1.toLong())
            return Input(blizzards,walls,party,goal)
        }
        fun parse2(input: String) = parse1(input)

        fun compute1(input: Input): Long =
            compute1(input, 150).first

        tailrec fun compute1(input: Input, top: Int): Pair<Long,List<Pair<XY,Dir>>> {
            println("Starting with top=$top")
            val possibleMoves: List<Dir?> = listOf(N,S,W,E,null)
            var step = 0L
            var currentBlizzards = input.blizzards
            var currentParties = setOf(input.party)
            val maxX = input.walls.maxOf { it.x }
            val maxY = input.walls.maxOf { it.y }
            while (currentParties.isNotEmpty() && currentParties.none { it == input.goal }) {
                currentBlizzards = stepBlizzards(currentBlizzards, input.walls)
                val nextParties = currentParties.flatMap { party ->
                    possibleMoves
                        .mapNotNull { dir ->
                            val wantedXY = if (dir == null) party else party.step(dir)
                            if (wantedXY in input.walls) null
                            else if (wantedXY in currentBlizzards.map { it.first }) null
                            else if (wantedXY.x !in 0..maxX || wantedXY.y !in 0..maxY) null
                            else wantedXY
                        }
                }
                currentParties = nextParties.sortedBy { it.manhattanDistanceTo(input.goal) }.take(top).toSet()
                step++
            }
            if (currentParties.isEmpty()) return compute1(input, top + 50)
            return step to currentBlizzards
        }

        fun compute2(input: Input): Long {
            val (minsThere, blizzardsThere) = compute1(input, 150)
            val (minsBack, blizzardsBack) = compute1(
                input.copy(
                    blizzards = blizzardsThere,
                    party = input.goal,
                    goal = input.party
                ), 150)
            val (minsThereAgain, _) = compute1(input.copy(blizzards = blizzardsBack), 150)
            return minsThere + minsBack + minsThereAgain
        }

        private fun draw(walls: Set<XY>, blizzards: List<Pair<XY,Dir>>, parties: Set<XY>) {
            for (y in (walls.minOf { it.y })..(walls.maxOf { it.y })) {
                for (x in (walls.minOf { it.x })..(walls.maxOf { it.x })) {
                    print(
                        if (x to y in walls) '#'
                        else if (x to y in parties) '█'
                        else {
                            val dir = blizzards.firstOrNull { it.first == x to y }?.second
                            when (dir) {
                                null -> '.'
                                W -> '<'
                                E -> '>'
                                N -> '^'
                                S -> 'v'
                            }
                        }
                    )
                }
                print("\n")
            }
        }

        private fun stepBlizzards(blizzards: List<Pair<XY,Dir>>, walls: Set<XY>): List<Pair<XY,Dir>> =
            blizzards.map {
                val (currentXY, dir) = it
                val wantedXY = currentXY.step(dir)
                val nextXY =
                    if (wantedXY in walls)
                        when (dir) {
                            N -> currentXY.x to (walls.maxOf { it.y } - 1)
                            S -> currentXY.x to (walls.minOf { it.y } + 1)
                            W -> (walls.maxOf { it.x } - 1) to currentXY.y
                            E -> (walls.minOf { it.x } + 1) to currentXY.y
                        }
                    else wantedXY
                nextXY to dir
            }
    }
}

private fun XY.step(dir: Dir): XY =
    when (dir) {
        N -> this.decY()
        S -> this.incY()
        W -> this.decX()
        E -> this.incX()
    }

fun XY.manhattanDistanceTo(other: XY): Long =
    abs(this.x - other.x) + abs(this.y - other.y)
