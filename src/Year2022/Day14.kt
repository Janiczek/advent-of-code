import Day14.Companion.Item.*

class Day14 {
    companion object {
        const val inputFile = "input202214.txt"

        private val start = 500 to 0
        enum class Item { Rock, Sand }

        // PARSE

        fun parse1(input: String): Map<XY,Item> =
            input.lines().flatMap {
                it.split(Regex(" -> "))
                    .map {
                        val ns = it.split(",")
                        ns[0].toInt() to ns[1].toInt()
                    }
                    .pairs { a,b -> linePoints(a,b) }
                    .flatten()
                    .map { it to Rock }
            }.toMap()

        fun parse2(input: String) = parse1(input)

        private fun linePoints(a: XY, b: XY): List<XY> =
            if (a.x == b.x) /* vertical line */ {
                val (min,max) = listOf(a.y,b.y).sorted()
                (min..max).map { a.x to it }
            }
            else /* horizontal line */ {
                val (min,max) = listOf(a.x,b.x).sorted()
                (min..max).map { it to a.y }
            }

        // COMPUTE

        fun compute1(input: Map<XY, Item>): Int {
            val bottom = input.keys.maxOf { it.y }
            return go(input, bottom, 0).count { it.value == Sand }
        }

        private tailrec fun go(input: Map<XY, Item>, bottom: Int, i: Int): Map<XY, Item> {
            if (i % 10 == 0) {
                println("--- $i ---")
                println(drawn(input))
            }
            if (input[start] == Sand) return input // filled!
            val inputWithSand = input + (start to Sand)
            val newGrid = goUnit(inputWithSand, start, bottom)
            return if (newGrid.toMap() == input) input else go(newGrid, bottom, i+1)
        }

        private tailrec fun goUnit(input: Map<XY, Item>, grain: XY, bottom: Int): Map<XY, Item> =
            if (grain.y > bottom) input - grain
            else {
                val newGrain =
                    if      (input[grain.incY()]        == null) grain.incY()        // down
                    else if (input[grain.incY().decX()] == null) grain.incY().decX() // down,left
                    else if (input[grain.incY().incX()] == null) grain.incY().incX() // down,right
                    else                                         grain               // no movement
                if (newGrain == grain) {
                    //println("Sand rested at $newGrain")
                    input
                }
                else                   goUnit(input - grain + (newGrain to Sand), newGrain, bottom)
            }

        private fun drawn(input: Map<XY,Item>) =
            input.draw(".") { when (it) {
                Rock -> "#"
                Sand -> "o"
            } }

        fun compute2(input: Map<XY,Item>): Int {
            val bounds = input.bounds()
            val inputWithGround = input + linePoints(
                bounds.minX - 360 to bounds.maxY + 2, // 369 should be the _max_ width of the whole thing, but just to be sure...
                bounds.maxX + 360 to bounds.maxY + 2,
            ).map { it to Rock }
            val result = go(inputWithGround, bounds.maxY + 2, 0)
            println("================")
            println(drawn(result))
            println("================")
            return result.count { it.value == Sand }
        }

    }
}

fun <T> T.log(label: String): T {
    println("$label: $this")
    return this
}

fun <T> List<T>.pairs(): List<Pair<T,T>> =
    this.pairs { a,b -> a to b }

fun <T, R> List<T>.pairs(transform: (T, T) -> R): List<R> =
    this.zip(this.drop(1), transform)

val <A, B> Pair<A, B>.x: A get() = this.first
val <A, B> Pair<A, B>.y: B get() = this.second

typealias XY = Pair<Int,Int>

fun XY.addX(n: Int): XY = this.x + n to this.y
fun XY.addY(n: Int): XY = this.x to this.y + n
fun XY.subX(n: Int): XY = this.addX(-n)
fun XY.subY(n: Int): XY = this.addY(-n)
fun XY.incX(): XY = this.addX(1)
fun XY.incY(): XY = this.addY(1)
fun XY.decX(): XY = this.subX(1)
fun XY.decY(): XY = this.subY(1)
fun XY.setX(x: Int): XY = x to this.y
fun XY.setY(y: Int): XY = this.x to y

data class Bounds(
    val minX: Int,
    val minY: Int,
    val maxX: Int,
    val maxY: Int,
)
fun <T> Map<XY,T>.bounds(): Bounds =
    Bounds(
      minX = this.keys.minOf { it.x },
      minY = this.keys.minOf { it.y },
      maxX = this.keys.maxOf { it.x },
      maxY = this.keys.maxOf { it.y },
    )
private fun <V> Map<XY,V>.draw(empty: String, item: (V) -> String): String {
    val bounds = this.bounds()
    return (bounds.minY..bounds.maxY).joinToString("\n") { y ->
        (bounds.minX..bounds.maxX).joinToString("") { x ->
            val i: V? = this[x to y]
            if (i != null) item(i)
            else empty
        }
    }
}

