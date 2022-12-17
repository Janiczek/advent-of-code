import Day17.Companion.Entity.*
import Day17.Companion.Wind.*

class Day17 {
    companion object {
        const val inputFile = "input202217.txt"

        enum class Wind { Left, Right }
        enum class Entity { Floor, Rock, MovingRock }

        private val rMinus  = parseShape("####")
        private val rPlus   = parseShape(".#.\n###\n.#.")
        private val rL      = parseShape("..#\n..#\n###")
        private val rI      = parseShape("#\n#\n#\n#")
        private val rSquare = parseShape("##\n##")

        private val rocks = listOf(rMinus, rPlus, rL, rI, rSquare)
        private const val minX = 0
        private const val maxX = 6

        private const val verbose = false

        private fun prn(a: Any) { if (verbose) println(a) }

        private fun parseShape(s: String): Set<XY> =
            s.lines().reversed().mapIndexed { y,line -> line.mapIndexedNotNull { x,c -> when (c) {
                '#' -> x.toLong() to y.toLong()
                else -> null
            } } }.flatten().toSet()

        fun parse1(input: String): List<Wind> =
            input.map { when (it) {
                '<' -> Left
                '>' -> Right
                else -> TODO("Parse - what?")
            } }
        fun parse2(input: String) = parse1(input)

        fun compute1(jet: List<Wind>): Long = go(2022L, jet)
        fun compute2(jet: List<Wind>): Long = /*-2*/ go(1000000000000L, jet)

        private fun floorAtY(y: Long): Map<XY,Entity> =
            (0L..6L).associate { (it to y) to Floor }

        private fun go(rocksCount: Long, jet: List<Wind>): Long {
            var board: MutableMap<XY,Entity> = floorAtY(-1).toMutableMap()
            var maxY = -1L
            var rockI = 0L
            var jetI = 0L
            var floorY = -1L
            var hasSkipped = false
            drw(board)
            val seenGarbage = mutableMapOf<Pair<Int,Int>,Pair<Long,Long>>()
            var skippedY = 0L
            while (rockI < rocksCount) {
                println("rock: $rockI, jet: $jetI, maxY: $maxY")

                //draw(board)
                val groupedByY = board.keys.groupBy { it.y }
                val fullRows = groupedByY.filter { it.value.size == 7 && it.key > floorY }.keys.toList()
                if (fullRows.isNotEmpty()) {
                    val fullRow = fullRows[0]
                    prn("Garbage collecting!")
                    board = board.filter { it.key.y >= fullRow }.toMutableMap()
                    board += floorAtY(fullRow)
                    floorY = fullRow
                    drw(board)

                    if (!hasSkipped) {
                        val jetToRock = jetI.mod(jet.size) to rockI.mod(rocks.size)
                        if (jetToRock in seenGarbage && !hasSkipped) {
                            // finish the damn thing!
                            val (oldMaxY, oldRockI) = seenGarbage[jetToRock]!!
                            val maxYCyclePeriod = maxY - oldMaxY
                            val rockICyclePeriod = rockI - oldRockI
                            println("HUGE: Jet/Rock ${jetToRock}, maxY: $oldMaxY -> $maxY (...$maxYCyclePeriod...), rockI: $oldRockI -> $rockI (...$rockICyclePeriod...)")
                            val rocksLeft = rocksCount - rockI
                            val rockCycles = rocksLeft / rockICyclePeriod
                            rockI += rockCycles * rockICyclePeriod
                            skippedY = rockCycles * maxYCyclePeriod
                            //board = board.mapKeys { it.key.addY(rockCycles * maxYCyclePeriod) }.toMutableMap()
                            //assert(maxY == board.maxOf { it.key.y + 1 }) { "Whoops we did something wrong with skipping" }
                            hasSkipped = true
                        }
                        seenGarbage += jetToRock to (maxY to rockI)
                    }
                }

                maxY = createRock(rockI, maxY, board)
                rockI++
                while (board.containsValue(MovingRock)) {
                    pushJet(jetI, jet, board)
                    jetI++
                    moveRockDown(board, floorY)
                    maxY = board.maxOf { it.key.y + 1 }
                }
            }
            return board.maxOf { it.key.y + 1 } + skippedY
        }

        private fun drw(board: Map<XY, Entity>) { if (verbose) draw(board) }
        private fun draw(board: Map<XY, Entity>) =
            println(board.draw(empty = ".", showBounds = false) { when (it) {
                Rock -> "#"
                MovingRock -> "@"
                Floor -> "="
            }}.lines().reversed().joinToString("\n"))

        private fun createRock(rockI: Long, maxY: Long, board: MutableMap<XY,Entity>): Long {
            prn("Rock $rockI begins falling")
            val rock = rocks[rockI.mod(rocks.size)]
            val dy = if (rockI == 0L) 4 else 3
            board += rock.map { it.addX(2).addY(maxY + dy) to MovingRock }
            drw(board)
            return maxY + dy + rock.maxOf { it.y }
        }

        private fun moveRockDown(board: MutableMap<XY,Entity>, floorY: Long) {
            val movingItems = board.filterValues { it == MovingRock }
            val movingXYs = movingItems.keys.sortedBy { it.y }
            val rockXYs = board.filterValues { it == Rock }.keys
            if (movingItems.any { it.key.subY(1) in rockXYs || it.key.y - 1 <= floorY }) {
                prn("Rock falls 1 unit, causing it to come to rest")
                movingXYs.forEach {
                    board -= it
                    board += it to Rock
                }
            }
            else {
                prn("Rock falls 1 unit")
                movingXYs.forEach {
                    board -= it
                    board += it.subY(1) to MovingRock
                }
            }
            drw(board)
        }

        private fun pushJet(jetI: Long, jet: List<Wind>, board: MutableMap<XY,Entity>) {
            val wind = jet[jetI.mod(jet.size)]
            val dx = when (wind) {
                Left -> -1L
                Right -> 1L
            }
            val movingItems = board.filterValues { it == MovingRock }
            val movingXYs = when (wind) {
                Left  -> movingItems.keys.sortedBy           { it.x }
                Right -> movingItems.keys.sortedByDescending { it.x }
            }
            if (movingItems.all { it.key.x + dx in minX..maxX && it.key.addX(dx) !in board.filter { it.value == Rock }.keys }) {
                prn("Jet of gas pushes rock $wind")
                movingXYs.forEach {
                    board -= it
                    board += it.addX(dx) to MovingRock
                }
            } else prn("Jet of gas pushes rock $wind, but nothing happens")
            drw(board)
        }
    }
}