class Day08 {
    companion object {
        const val inputFile = "input202208.txt"

        fun parse1(input: String) = input.lines().map { it.toList().map { it.digitToInt() } }
        fun parse2(input: String) = parse1(input)
        fun compute1(input: List<List<Int>>): Int {
            val w = input[0].size
            val h = input.size
            return input
                .mapIndexed { y,row -> row.mapIndexed { x,tree -> isTreeVisible(tree, x, y, input, w, h) } }
                .flatten().count { it }
        }
        fun compute2(input: List<List<Int>>): Int {
            val w = input[0].size
            val h = input.size
            return input
                .mapIndexed { y,row -> row.mapIndexed { x,tree -> scenicScore(tree, x, y, input, w, h) } }
                .flatten().maxOf { it }
        }

        private fun isTreeVisible(tree: Int, x: Int, y: Int, input: List<List<Int>>, w: Int, h: Int): Boolean {
            val left  = (0 until x).all       { input[y][it] < tree }
            val up    = (0 until y).all       { input[it][x] < tree }
            val right = ((x + 1) until w).all { input[y][it] < tree }
            val down  = ((y + 1) until h).all { input[it][x] < tree }
            return left || up || right || down
        }

        private fun scenicScore(tree: Int, x: Int, y: Int, input: List<List<Int>>, w: Int, h: Int): Int {
            val fix: (Int, IntProgression) -> Int = { n, range -> if (n == 0) 0 else if (n == range.count()) n else n + 1 }
            val iLeft  = x-1 downTo 0
            val iUp    = y - 1 downTo 0
            val iRight = (x + 1) until w
            val iDown  = (y + 1) until h
            val left  = fix(iLeft.takeWhile  { input[y][it] < tree }.size, iLeft)
            val up    = fix(iUp.takeWhile    { input[it][x] < tree }.size, iUp)
            val right = fix(iRight.takeWhile { input[y][it] < tree }.size, iRight)
            val down  = fix(iDown.takeWhile  { input[it][x] < tree }.size, iDown)
            return left * up * right * down
        }

    }
}
