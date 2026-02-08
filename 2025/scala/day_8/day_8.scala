import scala.io.Source
import scala.util.{Try, Success, Failure}

val inputFilePath: String = "./day_8/test_input"

type Coordinate = (Long, Long, Long)
type Circuit = List[Coordinate]

def getFileCoordenates(src_path: String): List[Coordinate] = {

  def readFile(path: String): List[String] = {
    val source = Source.fromFile(path)
    return try { source.getLines().toList.filter(_ != "") }
    finally { source.close() }
  }

  def parseCoordinate(c: String): Coordinate = {
    require(
      c.trim.split(",").forall(_.trim.toLongOption.isDefined),
      s"Invalid coordinate $c!"
    )

    c.trim.split(",").map(_.trim.toLong).toList match {
      case List(x, y, z) => (x, y, z)
      case _ =>
        throw new IllegalArgumentException(
          s"Expected 3 coordinates, got ${c.trim.split(",").length}"
        )
    }
  }

  val coordinates = readFile(src_path)
    .map((s) => s.trim.split("").filter(_.nonEmpty).mkString)
    .map(parseCoordinate(_))

  require(
    coordinates.size == coordinates.distinct.size,
    s"Invalid input file. There are duplicate coordinates on the input file!"
  )

  return coordinates
}

def getDistance(p0: Coordinate, p1: Coordinate): Double = {
  return math.sqrt(
    p0.toList.zip(p1.toList).map((a, b) => a - b).map((x) => x * x).sum
  )
}

def consume(
    sources: List[Circuit],
    targets: List[Coordinate]
): (List[Circuit], List[Coordinate]) = {

  def minSelfDistance(tgt: List[Coordinate]): (Double, Coordinate, Coordinate) = {
    return tgt.zipWithIndex
      .flatMap { case (c0, i) => tgt.drop(i + 1).map(c1 => (getDistance(c0, c1), c0, c1)) }
      .sortBy(_(0))
      .head
  }

  def minDirectionalDistance(
      src: Circuit,
      tgt: List[Coordinate]
  ): (Double, Coordinate, Coordinate) = {
    return src
      .flatMap((sc) => tgt.map((tc) => (getDistance(sc, tc), sc, tc)))
      .sortBy(_(0))
      .head
  }

  if (sources.size == 0) {
    val tgtMinDist = minSelfDistance(targets)
    return (
      List(tgtMinDist.drop(1).toList),
      targets.filter(t => !tgtMinDist.toList.drop(1).contains(t))
    )
  } else {

    val minLinkDir = sources.zipWithIndex
      .map((s, si) => (minDirectionalDistance(s, targets), si))
      .sortBy(_(0)(0))
      .head

    val minLinkSrc = sources.zipWithIndex
      .map(
        (s, si) => (sources.splitAt(si)(0) ++ sources.splitAt(si)(1).tail).zipWithIndex
          .map((ss, ssi) => )
      )

    if (targets.size > 1) {
      val tgtMinDist = minSelfDistance(targets)

      if (minLinkDir(0)(0) > tgtMinDist(0)) {
        return (
          sources :+ tgtMinDist.drop(1).toList,
          targets.filter(t => !tgtMinDist.toList.drop(1).contains(t))
        )
      } else {
        return (
          List(
            sources.take(minLinkDir(1)),
            List(sources(minLinkDir(1)) :+ minLinkDir(0).last),
            sources.drop(minLinkDir(1) + 1)
          ).flatten,
          targets.filter(t => (t != minLinkDir(0).last))
        )
      }
    } else if (targets.size == 1) {
      return (
        List(
          sources.take(minLinkDir(1)),
          List(sources(minLinkDir(1)) :+ minLinkDir(0).last),
          sources.drop(minLinkDir(1) + 1)
        ).flatten,
        List()
      )
    } else {
      return (sources, targets)
    }
  }
}

def iterate(
    circuits: List[Circuit],
    coordinates: List[Coordinate]
): (List[Circuit], List[Coordinate]) = {
  println("----------------------")
  val (newCircuits: List[Circuit], newCoordinates: List[Coordinate]) =
    consume(circuits, coordinates)

  println(s"\nNew iteration circuits:")
  newCircuits.zipWithIndex.foreach((c, i) => {
    println(s"  Circuit ${i}")
    c.foreach(cc => println(s"    ${cc}"))
  })

  println("\nNew iteration coordinates:")
  newCoordinates.foreach((c) => println(s"    ${c}"))

  return (newCircuits, newCoordinates)
}

def sortCoordinates(coordinates: List[Coordinate]): List[Coordinate] = {
  return coordinates.sortBy(_(2)).sortBy(_(1)).sortBy(_(0))
}

@main
def entry() = {
  var coordinates = getFileCoordenates(inputFilePath)
  var circuits: List[Circuit] = List()

  println("\nStarting coordinates:")
  coordinates.foreach((c) => println(s"    ${c}"))

  coordinates = sortCoordinates(coordinates)

  println("\nOrdered coordinates:")
  coordinates.foreach((c) => println(s"    ${c}"))

  // while (coordinates.nonEmpty) {
  //   val (newCircuits, newCoordinates) = iterate(circuits, coordinates)
  //   circuits = newCircuits
  //   coordinates = sortCoordinates(newCoordinates)
  // }

  for (i <- 1 to 100) {
    if (coordinates.nonEmpty) {
      val (newCircuits, newCoordinates) = iterate(circuits, coordinates)
      circuits = newCircuits
      coordinates = sortCoordinates(newCoordinates)
    }
  }

  println(
    // s"The product of the sizes of the tree biggest cuircuits is ${
    s"The tree biggest cuircuits are: ${circuits.map(_.size).sorted.takeRight(3) // .product
      }"
  )

}
