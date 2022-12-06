class Day06 {
    companion object {
        const val inputFile = "input202206.txt"

        fun parse1(input: String) = input
        fun parse2(input: String) = input
        fun compute1(input: String): Int = go(4,0,"",input)
        fun compute2(input: String): Int = go(14,0,"",input)

        private fun go(len: Int, n: Int, acc: String, rest: String): Int {
            if (acc.toSet().size == len) return n
            val next = rest[0]
            val newRest = rest.substring(1)
            val newAcc = (acc + next).takeLast(len)
            return go(len,n+1,newAcc,newRest)
        }
    }
}
