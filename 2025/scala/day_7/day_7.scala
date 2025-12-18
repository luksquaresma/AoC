import scala.io.Source
import scala.util.{Try, Success, Failure}

val inputFilePath: String = "./day_7/input"

def getFileLines(src_path: String): List[String] = {

  def readFile(path: String): List[String] = {
    val source = Source.fromFile(path)
    return try { source.getLines().toList.filter(_ != "") }
    finally { source.close() }
  }

  def parseLine(s: String): String = {
    val validChars = List("S", ".", "^", "|")

    require(
      validChars.exists(s.contains(_)),
      s"No valid chars found on line $s!"
    )

    require(
      s.map(_.toString).toSet.forall(validChars.contains(_)),
      s"Some invalid characters for line $s!"
    )

    return s.trim.split("").filter(_.nonEmpty).mkString
  }

  return readFile(src_path).map(parseLine)
}

def padGrid[T](grid: List[List[T]], padding: T): List[List[T]] = {
  return (List.fill(grid(0).length + 2)(padding) +: grid.map(padding +: _ :+ padding) :+ List.fill(
    grid(0).length + 2
  )(padding))
}

def splitBeamPath(lines: List[String]): List[String] = {
  var matrix = padGrid(lines.map(_.split("").toList).toList, ".")

  for (i <- 0 until matrix.length) {
    for (j <- 0 until matrix(i).length) {
      if ((i + 1) < matrix.length) {
        if ((matrix(i)(j) == "S") & (matrix(i + 1)(j) == ".")) {
          matrix = matrix.updated(i + 1, matrix(i + 1).updated(j, "|"))
        }
        if ((matrix(i)(j) == "|") & (matrix(i + 1)(j) == "^")) {
          if (j > 0) {
            matrix = matrix.updated(i + 1, matrix(i + 1).updated(j - 1, "|"))
          }
          if (j < (matrix(i).length - 1)) {
            matrix = matrix.updated(i + 1, matrix(i + 1).updated(j + 1, "|"))
          }
        }
        if ((matrix(i)(j) == "|") & (matrix(i + 1)(j) == ".")) {
          matrix = matrix.updated(i + 1, matrix(i + 1).updated(j, "|"))
        }
      }
    }
  }

  return matrix.map(_.mkString)
}

def countSplits(paddedGridlines: List[String]): Long = {
  val matrix = paddedGridlines.map(_.split("").toList).toList

  val cells = (1 until matrix.length - 1)
    .map((lin) =>
      (1 until matrix(0).length - 1)
        .map((col) => matrix.slice(lin - 1, lin + 1).map(_.slice(col - 1, col + 2)))
        .toList
    )
    .flatten
    .toList

  return cells
    .map((c) =>
      if ((c(0)(1) == "|") & (c(1)(1) == "^") & (c(1)(0) == "|") & (c(1)(2) == "|")) { 1 }
      else { 0 }
    )
    .sum()
}

@main
def entry() = {
  var lines = getFileLines(inputFilePath)
  var splitedPath = splitBeamPath(lines)

  // println("\nInitial map:")
  // lines.foreach(println(_))

  // println("\nFirst iteration map:")
  // splitedPath.foreach(println(_))

  println(s"Current number of splits == ${countSplits(splitedPath)}")

}
