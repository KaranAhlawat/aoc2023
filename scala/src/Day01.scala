import fs2.io.file.Files
import cats.effect.IO
import fs2.io.file.Path
import fs2.Stream
import cats.effect.IOApp

import utils.*

object Day01 extends IOApp.Simple:
  private def part1(input: Stream[IO, String]): Stream[IO, Int] =
    input
      .map: cal =>
        val digits = cal.filter(_.isDigit)
        s"${digits.head}${digits.last}".toInt
      .foldMonoid
      .head

  private def part2(input: Stream[IO, String]): Stream[IO, Int] =
    val digitRepr = Map(
      "one" -> 1,
      "two" -> 2,
      "three" -> 3,
      "four" -> 4,
      "five" -> 5,
      "six" -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine" -> 9
    ) ++ (1 to 9).map(i => i.toString() -> i)

    def getDigits(line: String): Int =
      val patt = "one|two|three|four|five|six|seven|eight|nine"
      val startPatt = (patt ++ "|\\d").r
      val endPatt = (patt.reverse ++ "|\\d").r
      val startDigit =
        startPatt
          .findFirstMatchIn(line)
          .fold(0)(m => digitRepr(m.matched))
      val lastDigit =
        endPatt
          .findFirstMatchIn(line.reverse)
          .fold(0)(m => digitRepr(m.matched.reverse))

      startDigit * 10 + lastDigit

    input
      .map(getDigits)
      .foldMonoid
      .head

  def run: IO[Unit] =
    val input = readInput("Day01")

    part1(input)
      .zip(part2(input))
      .evalTap(IO.println)
      .compile
      .drain
