fun main() {
  val input = util.readInput("Day01.txt")

  fun part1(input: List<String>): Int =
      input.sumOf { cal ->
        cal.first { it.isDigit() }.digitToInt() * 10 + cal.last { it.isDigit() }.digitToInt()
      }

  fun part2(input: List<String>): Int {
    val reprToDigit =
        mapOf(
            "one" to 1,
            "two" to 2,
            "three" to 3,
            "four" to 4,
            "five" to 5,
            "six" to 6,
            "seven" to 7,
            "eight" to 8,
            "nine" to 9
        ) + (1..9).map { it.toString() to it }

    fun numberFromLine(line: String): Int {
      val capturingRegex = ("(?=(" + reprToDigit.keys.joinToString("|") + "))").toRegex()
      val matches = capturingRegex.findAll(line).map { it.groupValues[1] }.toList()
      val firstDigit = reprToDigit[matches.first()]!!
      val lastDigit = reprToDigit[matches.last()]!!

      return firstDigit * 10 + lastDigit
    }

    return input.sumOf { numberFromLine(it) }
  }

  part1(input).dbg()
  part2(input).dbg()
}
