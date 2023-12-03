import java.io.File

object util {
  fun readInput(path: String): List<String> = File("src/main/kotlin/inputs/$path").readLines()
}

fun Any?.dbg() = println(this)
