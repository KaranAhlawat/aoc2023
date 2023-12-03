import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import fs2.Stream
import utils.readInput

val test = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
""".stripMargin

case class Tile(n: String, start: Int, stop: Int, row: Int)

def buildGrid(input: Stream[IO, String]) =
  input.zipWithIndex
    .map((line, idx) =>
      "(\\d+)|([^.\\d])".r
        .findAllMatchIn(line)
        .map(n => Tile(n.matched, n.start, n.end - 1, idx.toInt))
        .toList
    )
    .foldMonoid
    .flatMap(x => Stream.emits(x))

def checkAdjacency(sym: Tile, num: Tile): Boolean =
  val firstCond = sym.start == num.start - 1 ||
    sym.start == num.stop + 1 ||
    (sym.start >= num.start - 1 && sym.start <= num.stop + 1)

  val secondCond = sym.row >= num.row - 1 && sym.row <= num.row + 1

  firstCond && secondCond

def tileIsValid(tile: Tile, syms: Vector[Tile]) =
  syms
    .filter(s => checkAdjacency(s, tile))
    .nonEmpty

def getGearRatio(s: Tile, numberTiles: Vector[Tile]) =
  val ratioTiles = numberTiles
    .filter(tile => checkAdjacency(s, tile))

  if (ratioTiles.size == 2) then ratioTiles.map(_.n.toInt).product else 0

def part1(grid: IO[(Vector[Tile], Vector[Tile])]) =
  grid
    .map((symTiles, numTiles) =>
      numTiles
        .map(tile => if tileIsValid(tile, symTiles) then tile.n.toInt else 0)
        .sum
    )

def part2(grid: IO[(Vector[Tile], Vector[Tile])]) =
  grid
    .map((symTiles, numTiles) =>
      symTiles
        .filter(_.n == "*")
        .map(tile => getGearRatio(tile, numTiles))
        .sum
    )

object Day03 extends IOApp.Simple:
  override def run: IO[Unit] =
    val input = readInput("Day03")
    val testStream = Stream.emits(test.split('\n'))
    val grid = buildGrid(input).compile.toVector.map(tiles =>
      val symTiles =
        tiles.filter(t => t.n.length() == 1 && !t.n(0).isDigit)
      val numTiles =
        tiles.filterNot(t => t.n.length() == 1 && !t.n(0).isDigit)
      (symTiles, numTiles)
    )

    part1(grid).flatMap(one => part2(grid).flatMap(two => IO.println(one, two)))
