import kotlin.math.abs
import kotlin.math.pow
import kotlin.math.sqrt

class Day18 {
    companion object {
        const val inputFile = "input202218.txt"

        fun parse1(input: String): List<XYZ> = input.lines().map { parseLine(it) }
        fun parse2(input: String) = parse1(input)

        private fun parseLine(line: String): XYZ {
            val ints = line.split(",").map { it.toInt() }
            val (x,y,z) = ints
            return Triple(x,y,z)
        }

        fun compute1(input: List<XYZ>): Int =
            allSides(input).size

        fun compute2(input: List<XYZ>): Int {
            val set = input.toSet()
            val min = input.flatMap { it.toList() }.minOf { it } - 1
            val max = input.flatMap { it.toList() }.maxOf { it } + 1
            val range = min..max
            val start = Triple(min,min,min)
            val todos = mutableSetOf(start)
            val outside = mutableSetOf<XYZ>()
            val seen = mutableSetOf<XYZ>()
            while (todos.isNotEmpty()) {
                val todo = todos.first()
                todos -= todo
                val neighbours = todo.neighbours().filter { it.x in range && it.y in range && it.z in range }
                if (neighbours.any { it in set }) outside += todo
                todos += neighbours.filter { it !in set && it !in seen }
                seen += todo
            }
            return allSides(seen.toList()).intersect(allSides(input)).size
        }

        private fun sides(point: XYZ): List<Set<XYZ>> {
            val (a,b,c) = point
            val m = Triple(a,b,c)
            val n = Triple(a,b,c+1)
            val o = Triple(a+1,b,c+1)
            val p = Triple(a+1,b,c)
            val q = Triple(a,b+1,c)
            val r = Triple(a,b+1,c+1)
            val s = Triple(a+1,b+1,c+1)
            val t = Triple(a+1,b+1,c)
            return listOf(
                setOf(m,n,o,p),
                setOf(m,n,r,q),
                setOf(n,o,s,r),
                setOf(o,p,t,s),
                setOf(p,m,q,t),
                setOf(q,r,s,t),
            )
        }

        private fun allSides(input: List<XYZ>): Set<Set<XYZ>> =
            input.flatMap { sides(it) }.frequencies().filterValues { it == 1 }.keys
    }
}

typealias XYZ = Triple<Int,Int,Int>

val XYZ.x: Int get() { return first }
val XYZ.y: Int get() { return second }
val XYZ.z: Int get() { return third }

fun XYZ.addX(x: Int): XYZ = this.copy(first = this.first + x)
fun XYZ.addY(y: Int): XYZ = this.copy(second = this.second + y)
fun XYZ.addZ(z: Int): XYZ = this.copy(third = this.third + z)

fun XYZ.neighbours(): Set<XYZ> =
    setOf(
        this.addX(1),
        this.addX(-1),
        this.addY(1),
        this.addY(-1),
        this.addZ(1),
        this.addZ(-1),
    )

fun XYZ.euclideanDistanceTo(other: XYZ): Double =
    sqrt(
        (this.x - other.x).toDouble().pow(2)
                + (this.y - other.y).toDouble().pow(2)
                + (this.z - other.z).toDouble().pow(2)
    )

fun XYZ.manhattanDistanceTo(other: XYZ): Int =
    abs(this.x - other.x) +
            abs(this.y - other.y)  +
            abs(this.z - other.z)

fun <T> List<T>.frequencies(): Map<T,Int> =
    this.groupingBy { it }.eachCount()