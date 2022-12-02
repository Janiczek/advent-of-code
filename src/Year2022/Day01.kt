class Day01 {
    companion object {
        const val inputFile = "input202201.txt"

        fun parse1(input: String) =
            input
                .split("\n\n")
                .map { it.lines().map { it.toInt() } }

        fun parse2(input: String) = parse1(input)

        fun compute1(input: List<List<Int>>) =
            input.maxOf { it.sum() }

        fun compute2(input: List<List<Int>>) =
            input
                .map { it.sum() }.sortedDescending()
                .take(3).sum()
    }
}
