class Day04 {
    companion object {
        const val inputFile = "input202204.txt"

        fun parse1(input: String) =
            input.lines().map {
                val list = it.split(",").map { it.split("-") }
                (list[0][0].toInt() to list[0][1].toInt()) to (list[1][0].toInt() to list[1][1].toInt())
            }
        fun parse2(input: String) = parse1(input)
        fun compute1(input: List<Pair<Pair<Int,Int>,Pair<Int,Int>>>) =
            input.count { containsFully(it.first,it.second) || containsFully(it.second,it.first) }
        fun compute2(input: List<Pair<Pair<Int,Int>,Pair<Int,Int>>>) =
            input.count { contains(it.first,it.second) || contains(it.second,it.first) }

        private fun containsFully(big: Pair<Int, Int>, small: Pair<Int, Int>) =
            big.first <= small.first && big.second >= small.second
        private fun contains(big: Pair<Int, Int>, small: Pair<Int, Int>) =
            big.first <= small.first && big.second >= small.first
    }
}
