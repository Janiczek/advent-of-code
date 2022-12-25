import java.lang.Math.pow

class Day25 {
    companion object {
        const val inputFile = "input202225.txt"

        fun parse1(input: String): List<String> = input.lines()
        fun parse2(input: String) = parse1(input)

        // Do the rest with Mathematica:
        // FindInstance[n = a*5^0 + b*5^1 + ... && -2<=a<=2 && -2<=b<=2 && ..., {a,b,...}, Integers]
        fun compute1(input: List<String>): Long = input.sumOf { convertFromSnafu(it) }
        fun compute2(input: List<String>): String = "-3"

        private fun convertFromSnafu(line: String): Long =
            line.reversed().mapIndexed { i,c ->
                pow(5.0,i.toDouble()).toLong() * when (c) {
                    '2' -> 2
                    '1' -> 1
                    '0' -> 0
                    '-' -> -1
                    '=' -> -2
                    else -> TODO("huh")
                }
            }.sum()
    }
}
