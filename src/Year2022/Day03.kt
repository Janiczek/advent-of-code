class Day03 {
    companion object {
        const val inputFile = "input202203.txt"

        fun parse1(input: String): List<Pair<String,String>> =
            input.lines().map {
                val len = it.length
                val len2 = len/2
                val fst = it.substring(0,len2)
                val snd = it.substring(len2)
                fst to snd
            }
        fun parse2(input: String) =
            input.lines().chunked(3)
        fun compute1(input: List<Pair<String,String>>) =
            input.sumOf { compute1Single(it) }
        fun compute2(input: List<List<String>>) =
            input.sumOf { compute2Group(it) }

        private fun compute1Single(pair: Pair<String,String>): Int {
            val (fst,snd) = pair
            val char = fst.filter { snd.contains(it) }[0]
            return priority(char)
        }
        private fun compute2Group(group: List<String>): Int {
            val sets = group.map { it.toSet() }
            val common = sets.reduce { a,b -> a.intersect(b) }
            return priority(common.first())
        }
        private fun priority(char: Char) =
            if (char.isUpperCase())
                char.code - 'A'.code + 27
            else
                char.code - 'a'.code + 1
    }
}
