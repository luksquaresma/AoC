import scala.io.Source
import scala.util.{Try, Success, Failure}

val inputFilePath: String = "./day_4/input"

def getFileGrid(src_path: String): List[List[String]] = {

  def readFile(path: String): List[String] = {
    val source = Source.fromFile(path)
    return try { source.getLines().toList.filter(_ != "") }
    finally { source.close() }
  }

  def parseSymbol(c: String): String = {
    require(
      List(".", "@").contains(c),
      s"Invalid input for char $c!"
    )
    return c
  }

  return readFile(src_path).map((line) => line.split("").toList.filter(_ != "").map(parseSymbol))
}

def padGrid[T](grid: List[List[T]], padding: T): List[List[T]] = {
  return (List.fill(grid(0).length + 2)(padding) +: grid.map(padding +: _ :+ padding) :+ List.fill(
    grid(0).length + 2
  )(padding))
}

def calculateAdjacent(paddedGrid: List[List[String]]): List[List[Int]] = {
  val cells = (1 until paddedGrid.length - 1)
    .map((lin) =>
      (1 until paddedGrid(0).length - 1)
        .map((col) => paddedGrid.slice(lin - 1, lin + 2).map(_.slice(col - 1, col + 2)))
        .toList
    )
    .toList

  val adj = cells.map(
    _.map((cell) =>
      cell.zipWithIndex
        .flatMap((le, li) =>
          le.zipWithIndex.flatMap((ce, ci) =>
            if (!(li == 1 && ci == 1)) {
              if (ce == "@") { List(1) }
              else List(0)
            } else { List(0) }
          )
        )
        .sum
    )
  )

  return adj
}

def checkMoovable(
    grid: List[List[String]],
    adj: List[List[Int]],
    element: String
): List[List[String]] = {

  return grid
    .zip(adj)
    .map((gl, al) =>
      gl.zip(al)
        .map((g, a) =>
          if ((g == element) && (a < 4)) { "x" }
          else { g }
        )
    )
}

@main
def entry() = {
  var grid = getFileGrid(inputFilePath)
  var paddedGrid = padGrid(grid, ".")
  val adjacents = calculateAdjacent(paddedGrid)
  val moovableMap = checkMoovable(grid, adjacents, "@")

  // println("\nCurrent grid:")
  // grid.foreach((l) => println(l.mkString))

  // println("\nPadded grid:")
  // paddedGrid.foreach((l) => println(l.mkString))

  // println("\nAdjacent grid:")
  // adjacents.foreach((l) => println(l.mkString))

  // println("\nMoovable map:")
  // moovableMap.foreach((l) => println(l.mkString))

  println(s"\nTotal moovable = ${moovableMap.flatten.count(_ == "x")}")
}
