import Day02.Companion.Hand.*
import Day02.Companion.Outcome.*

class Day02 {
    companion object {
        const val inputFile = "input202202.txt"

        fun parse1(input: String): List<List<String>> =
            input.lines().map { it.split(" ") }

        fun parse2(input: String) = parse1(input)

        fun compute1(input: List<List<String>>) =
            input.sumOf { scoreRound1(it) }

        fun compute2(input: List<List<String>>) =
            input.sumOf { scoreRound2(it) }

        enum class Hand { Rock, Paper, Scissors }
        enum class Outcome { Lose, Draw, Win }

        private fun scoreRound1(round: List<String>): Int {
            val elf = elfHand[round[0]]!!
            val you = p1Hand[round[1]]!!
            val handScore = handScores[you]!!
            val outcome = outcomeFromHands[elf to you]!!
            val outcomeScore = outcomeScores[outcome]!!
            return handScore + outcomeScore
        }

        private fun scoreRound2(round: List<String>): Int {
            val elf = elfHand[round[0]]!!
            val outcome = p2Outcome[round[1]]!!
            val outcomeScore = outcomeScores[outcome]!!
            val you = handFromOutcome[elf to outcome]!!
            val handScore = handScores[you]!!
            return handScore + outcomeScore
        }

        private val elfHand = mapOf("A" to Rock, "B" to Paper, "C" to Scissors)
        private val p1Hand  = mapOf("X" to Rock, "Y" to Paper, "Z" to Scissors)
        private val p2Outcome = mapOf("X" to Lose, "Y" to Draw, "Z" to Win)
        private val handScores = mapOf(Rock to 1, Paper to 2, Scissors to 3)
        private val outcomeScores = mapOf(Lose to 0, Draw to 3, Win to 6)
        private val outcomeFromHands = mapOf( // (Elf to You) to Outcome
            (Rock     to Rock) to Draw, (Rock     to Paper) to Win,  (Rock     to Scissors) to Lose,
            (Paper    to Rock) to Lose, (Paper    to Paper) to Draw, (Paper    to Scissors) to Win,
            (Scissors to Rock) to Win,  (Scissors to Paper) to Lose, (Scissors to Scissors) to Draw,
        )
        private val handFromOutcome = mapOf( // (Elf to Outcome) to You
            (Rock     to Draw) to Rock, (Rock     to Win)  to Paper, (Rock     to Lose) to Scissors,
            (Paper    to Lose) to Rock, (Paper    to Draw) to Paper, (Paper    to Win)  to Scissors,
            (Scissors to Win)  to Rock, (Scissors to Lose) to Paper, (Scissors to Draw) to Scissors,
        )
    }
}
