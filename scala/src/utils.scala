import cats.effect.IO
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path

object utils:
  def readInput(path: String): Stream[IO, String] =
    Files[IO].readUtf8Lines(Path(s"inputs/$path.txt"))
