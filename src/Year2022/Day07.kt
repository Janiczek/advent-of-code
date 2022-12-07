class Day07 {
    companion object {
        const val inputFile = "input202207.txt"

        sealed class FsEntry
        data class Dir(val name: String): FsEntry()
        data class File(val name: String, val size: Int): FsEntry()

        fun parse1(input: String): Map<String,List<FsEntry>> {
            fun go(
                accPath: List<String>,
                accEntries: List<FsEntry>,
                accMap: Map<String,List<FsEntry>>,
                lines: List<String>
            ): Pair<Map<String,List<FsEntry>>,List<String>> {
                // base cases
                if (lines.isEmpty() || lines[0] == "$ cd ..") {
                    return if (accPath == listOf("")) {
                        // finish!
                        Pair(accMap + ("/" to accEntries), lines.drop(1))
                    } else {
                        val path = accPath.joinToString("/")
                        val newMap = accMap + (path to accEntries)
                        // finish nested case
                        Pair(newMap, lines.drop(1))
                    }
                }
                // recursive cases
                val line = lines[0]
                if (line == "$ ls") return go(accPath, accEntries, accMap, lines.drop(1))
                if (line.startsWith("$ cd ")) {
                    val name = line.substring(5)
                    val (entries, rest) = go(accPath + name, listOf(), mapOf(), lines.drop(1))
                    return go(accPath, accEntries, accMap + entries, rest)
                }
                if (line.startsWith("dir ")) {
                    val name = line.substring(4)
                    return go(accPath, accEntries + Dir(name), accMap, lines.drop(1))
                }
                // only possibility left is the file entry
                val split = line.split(" ")
                val size = split[0].toInt()
                val name = split[1]
                return go(accPath, accEntries + File(name, size), accMap, lines.drop(1))
            }
            val (map, rest) = go(listOf(""),listOf(),mapOf(),input.lines().drop(1)) // special casing for `cd /`
            assert(rest.isEmpty())
            return map
        }

        fun parse2(input: String) = parse1(input)

        fun compute1(input: Map<String,List<FsEntry>>): Int {
            val dirSizes = computeDirSizes(input)
            return dirSizes.filterValues { it <= 100_000 }.values.sum()
        }

        fun compute2(input: Map<String,List<FsEntry>>): Int {
            val dirSizes = computeDirSizes(input)
            val total = 70_000_000
            val needed = 30_000_000
            val maxUsed = total - needed
            val used = dirSizes["/"]!!
            val bigEnoughDirs = dirSizes.filterValues { used - it <= maxUsed }
            val dirToDelete = bigEnoughDirs.minBy { it.value }
            return dirToDelete.value
        }

        private fun computeDirSizes(input: Map<String,List<FsEntry>>): Map<String,Int> {
            fun go(accDirSizes: Map<String,Int>, cwd: List<String>, todo: List<String>): Map<String,Int> {
                if (todo.isEmpty()) {
                    return accDirSizes
                }
                val path =
                    if (cwd == listOf("")) "/"
                    else cwd.joinToString("/")
                val newTodo = todo.drop(1)
                if (accDirSizes.containsKey(path)) {
                    // already done!
                    return go(accDirSizes, cwd, newTodo)
                }
                val entries = input[path]!!
                val (newDirSizes, thisSize) = entries.fold(Pair(accDirSizes, 0)) { (currentDirSizes, accSize), entry ->
                    when (entry) {
                        is File -> Pair(currentDirSizes, accSize + entry.size)
                        is Dir -> {
                            val newCwd = cwd + entry.name
                            val resolved = go(currentDirSizes, newCwd, listOf(entry.name))
                            val pathWithName = newCwd.joinToString("/")
                            val newDirSize = resolved[pathWithName]!!
                            Pair(resolved, accSize + newDirSize)
                        }
                    }
                }
                return go(newDirSizes + (path to thisSize), cwd, newTodo)
            }
            return go(mapOf(),listOf(""),input.keys.toList())
        }
    }
}
