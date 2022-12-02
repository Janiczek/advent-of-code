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

        private val elfHand = mapOf("A" to Hand.Rock, "B" to Hand.Paper, "C" to Hand.Scissors)
        private val p1Hand  = mapOf("X" to Hand.Rock, "Y" to Hand.Paper, "Z" to Hand.Scissors)
        private val p2Outcome = mapOf("X" to Outcome.Lose, "Y" to Outcome.Draw, "Z" to Outcome.Win)
        private val handScores = mapOf(Hand.Rock to 1, Hand.Paper to 2, Hand.Scissors to 3)
        private val outcomeScores = mapOf(Outcome.Lose to 0, Outcome.Draw to 3, Outcome.Win to 6)
        private val outcomeFromHands = mapOf( // (Elf to You) to Outcome
            (Hand.Rock     to Hand.Rock) to Outcome.Draw, (Hand.Rock     to Hand.Paper) to Outcome.Win,  (Hand.Rock     to Hand.Scissors) to Outcome.Lose,
            (Hand.Paper    to Hand.Rock) to Outcome.Lose, (Hand.Paper    to Hand.Paper) to Outcome.Draw, (Hand.Paper    to Hand.Scissors) to Outcome.Win,
            (Hand.Scissors to Hand.Rock) to Outcome.Win,  (Hand.Scissors to Hand.Paper) to Outcome.Lose, (Hand.Scissors to Hand.Scissors) to Outcome.Draw,
        )
        private val handFromOutcome = mapOf( // (Elf to Outcome) to You
            (Hand.Rock     to Outcome.Draw) to Hand.Rock, (Hand.Rock     to Outcome.Win)  to Hand.Paper, (Hand.Rock     to Outcome.Lose) to Hand.Scissors,
            (Hand.Paper    to Outcome.Lose) to Hand.Rock, (Hand.Paper    to Outcome.Draw) to Hand.Paper, (Hand.Paper    to Outcome.Win)  to Hand.Scissors,
            (Hand.Scissors to Outcome.Win)  to Hand.Rock, (Hand.Scissors to Outcome.Lose) to Hand.Paper, (Hand.Scissors to Outcome.Draw) to Hand.Scissors,
        )
    }
}
