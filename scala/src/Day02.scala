import cats.effect.IOApp
import cats.effect.IO
import utils.readInput
import fs2.Stream

object Day02 extends IOApp.Simple:
  val MAX_RED = 12
  val MAX_GREEN = 13
  val MAX_BLUE = 14

  private def parseToGame(str: String): Array[Map[String, Int]] =
    str
      .split(";")
      .map(
        _.split(",")
          .map(_.strip)
          .map(_ match
            case s"$n $color" => color -> n.toInt
          )
          .toMap
          .withDefaultValue(0)
      )

  private def mapLinesToSum(
      lines: Stream[IO, String],
      f: (Int, String) => Int
  ): Stream[IO, Int] =
    lines
      .map(_ match
        case s"Game $id: $rest" => f(id.toInt, rest)
        case _                  => 0
      )
      .foldMonoid
      .head

  private def part1(input: Stream[IO, String]) =
    def idIfPossible(id: Int, game: String): Int =
      val parsed = parseToGame(game)
      if parsed
          .filter(_("red") <= MAX_RED)
          .filter(_("green") <= MAX_GREEN)
          .filter(_("blue") <= MAX_BLUE)
          .length == parsed.length
      then id
      else 0

    mapLinesToSum(input, idIfPossible)

  private def part2(input: Stream[IO, String]) =
    def power(id: Int, game: String): Int =
      val parsed = parseToGame(game)
      parsed.map(_("red")).max *
        parsed.map(_("green")).max *
        parsed.map(_("blue")).max

    mapLinesToSum(input, power)

  def run: IO[Unit] =
    val input = readInput("Day02")

    part1(input)
      .zip(part2(input))
      .evalTap(IO.println)
      .compile
      .drain
