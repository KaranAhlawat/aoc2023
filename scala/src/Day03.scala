import cats.effect.IOApp
import cats.effect.IO
import cats.implicits.*
import fs2.Stream
import utils.readInput
import cats.syntax.all.*

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

sealed trait Tile:
  val pos: Int
  val row: Int

case class PartNum(n: Int, pos: Int, end: Int, row: Int) extends Tile
case class Symbol(c: Char, pos: Int, row: Int) extends Tile
case class Empty(pos: Int, row: Int) extends Tile

def buildGrid(input: Stream[IO, String]) =
  input.zipWithIndex
    .map((line, idx) =>
      val numRegex = "(\\d+)".r
      val symRegex = "([^.\\d])".r

      val nums = numRegex
        .findAllMatchIn(line)
        .map(n => PartNum(n.matched.toInt, n.start, n.end - 1, idx.toInt))
        .toList
        .sortBy(_.pos)

      val syms = symRegex
        .findAllMatchIn(line)
        .map(n => Symbol(n.matched(0), n.start, idx.toInt))

      (nums ++ syms).sortBy(_.pos)
    )
    .foldMonoid
    .flatMap(x => Stream.emits(x))

def checkAdjacency(sym: Symbol, num: PartNum): Boolean =
  val firstCond = sym.pos == num.pos - 1 ||
  sym.pos == num.end + 1 ||
  (sym.pos >= num.pos - 1 && sym.pos <= num.end + 1)
  
  val secondCond = sym.row >= num.row - 1 && sym.row <= num.row + 1
  
  firstCond && secondCond

def tileIsValid(tile: PartNum, syms: Vector[Symbol]) =
  syms
    .filter(s => checkAdjacency(s, tile) )
    .nonEmpty

def getGearRatio(s: Symbol, numberTiles: Vector[PartNum]) =
  val ratioTiles = numberTiles
    .filter(tile => checkAdjacency(s, tile) )

  if (ratioTiles.size == 2) then ratioTiles.map(_.n).product else 0

def part1(grid: IO[Vector[Tile]]) =
  grid
    .map(tiles =>
      val numberTiles =
        tiles.filter(_.isInstanceOf[PartNum]).asInstanceOf[Vector[PartNum]]
      val symTiles =
        tiles.filter(_.isInstanceOf[Symbol]).asInstanceOf[Vector[Symbol]]

      numberTiles
        .map(tile => if tileIsValid(tile, symTiles) then tile.n else 0)
        .sum
    )

def part2(grid: IO[Vector[Tile]]) =
  grid
    .map(tiles =>
      val numberTiles =
        tiles.filter(_.isInstanceOf[PartNum]).asInstanceOf[Vector[PartNum]]
      val symTiles =
        tiles.filter(_.isInstanceOf[Symbol]).asInstanceOf[Vector[Symbol]]

      symTiles
        .filter(_.c == '*')
        .map(tile => getGearRatio(tile, numberTiles))
        .sum
    )

object Day03 extends IOApp.Simple:
  override def run: IO[Unit] =
    val input = readInput("Day03")
    val testStream = Stream.emits(test.split('\n'))
    val grid = buildGrid(input).compile.toVector

    part1(grid).flatMap(one =>
      part2(grid).flatMap(two =>
        IO.println(one, two)
      )
    )
