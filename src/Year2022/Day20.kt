class Day20 {
    companion object {
        const val inputFile = "input202220.txt"

        fun parse1(input: String): List<Long> = input.lines().map { it.toLong() }
        fun parse2(input: String) = parse1(input)

        fun compute1(input: List<Long>): Long {
            val listWithIndexes = input.mapIndexed { i,n -> n to i.toLong() }
            val mixed = mix(listWithIndexes, listWithIndexes, 1)
            val zero = mixed.indexOfFirst { it.first == 0L }
            return mixed[(zero+1000)%input.size].first + mixed[(zero+2000)%input.size].first + mixed[(zero+3000)%input.size].first
        }
        fun compute2(input: List<Long>): Long {
            val key = 811589153L
            val listWithIndexes: List<Pair<Long,Long>> = input.mapIndexed { i,n -> n to i.toLong() }
            var mixed = listWithIndexes
            println("initial: ${mixed.map { it.first * key }}")
            for (i in 1..10) {
                println("mix $i/10")
                mixed = mix(mixed, listWithIndexes, key)
                println("after mix: ${mixed.map { it.first * key }}")
            }
            val encrypted = mixed.map { it.first * key }
            val zero = encrypted.indexOfFirst { it == 0L }
            return encrypted[(zero+1000)%input.size] + encrypted[(zero+2000)%input.size] + encrypted[(zero+3000)%input.size]
        }

        private fun mix(list: List<Pair<Long,Long>>, order: List<Pair<Long,Long>>, k: Long): List<Pair<Long,Long>> {
            return order.fold(list) { accList, x ->
                val (n,_) = x
                val nn = n * k
                val index = accList.indexOf(x)
                val newIndex1 = (index + nn).mod(list.size - 1)
                val newIndex = if (index != 0 && newIndex1 == 0) list.size - 1 else newIndex1
                val withoutN = accList.take(index) + accList.drop(index + 1)
                withoutN.take(newIndex) + listOf(x) + withoutN.drop(newIndex)
            }
        }
    }
}