fun main() {
    val input = util.readInput("Day02.txt")

    fun lineToGame(line: String): List<Map<String, Int>> {
        return line.split(";").map {
            it.split(",")
                .map { it.trim() }
                .map {
                    val (n, color) = it.split(" ")
                    color to n.toInt()
                }
                .toMap()
        }
    }

    fun part1(input: List<String>): Int {
        fun checkGame(game: List<Map<String, Int>>): Boolean =
            game
                .filter {
                    val n = it["red"] ?: 0
                    n <= 12
                }
                .filter {
                    val n = it["green"] ?: 0
                    n <= 13
                }
                .filter {
                    val n = it["blue"] ?: 0
                    n <= 14
                }
                .size == game.size

        return input.sumOf {
            val colonPos = it.indexOf(':') 
            val id = it.substring(5, colonPos)
            val rest = it.substring(colonPos + 1)
            val game = lineToGame(rest)
            if (checkGame(game)) id.toInt() else 0
        }
    }

    fun part2(input: List<String>): Int {
        fun minimumCubes(game: List<Map<String, Int>>): Int =
            game.map { it.get("red") ?: 1 }.max() *
                game.map { it.get("green") ?: 1 }.max() *
                game.map { it.get("blue") ?: 1 }.max()

        return input.sumOf {
            val colonPos = it.indexOf(':') 
            val rest = it.substring(colonPos + 1)
            val game = lineToGame(rest)
            minimumCubes(game)
        }
    }

    part1(input).dbg()
    part2(input).dbg()
}
