import java.math.BigInteger
import kotlin.math.abs

class Day15 {
    companion object {
        const val inputFile = "input202215.txt"

        val exampleLine = 10
        val realLine = 2000000

        val exampleMax = 20
        val realMax = 4000000

        val line = realLine
        val max = realMax

        data class Input(
            val list: List<Pair<XY,XY>>
        )

        fun parse1(input: String): Input = Input(input.lines().map { parseLine(it) })
        fun parse2(input: String) = parse1(input)
        private fun parseLine(line: String): Pair<XY,XY> {
            val ints = line.split(" ").filter { it.any { it.isDigit() }}.map { it.filter { it.isDigit() }.toInt() }
            return (ints[0] to ints[1]) to (ints[2] to ints[3])
        }

        fun compute1(input: Input): Int {
            val beacons = input.list.filter { it.second.y == line }.map { it.second.x }.toSet()
            val positions = input.list.fold(setOf<Int>()) { acc,reading -> acc.union(positionsAt(line, reading)) }
            return (positions - beacons).size
        }

        private fun positionsAt(line: Int, reading: Pair<XY,XY>): Set<Int> {
            val dist = reading.manhattanDistance()
            return if (line in (reading.first.y - dist)..(reading.first.y + dist)) {
                val lineDist = abs(reading.first.y - line)
                val toEachSide = dist - lineDist
                val range = (reading.first.x - toEachSide)..(reading.first.x + toEachSide)
                range.toSet()
            } else {
                setOf()
            }
        }

        fun compute2(input: Input): BigInteger {
            TODO("Implement that")
        }
    }
}

operator fun BigInteger.times(i: Int): BigInteger =
    this.times(i.toBigInteger())

operator fun BigInteger.plus(i: Int): BigInteger =
    this.plus(i.toBigInteger())

private fun IntRange.times(other: IntRange): List<Pair<Int,Int>> =
    this.flatMap { a -> other.map { b -> a to b } }

private fun Pair<XY, XY>.manhattanDistance(): Int =
    abs(this.first.x - this.second.x) + abs(this.first.y - this.second.y)
