import scala.io.Source
import scala.util.{Try, Success, Failure}

val inputFilePath: String = "./day_2/input"

def getFileRanges(src_path: String): List[List[Long]] = {

  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    return try { source.mkString }
    finally { source.close() }
  }

  def parseRange(s: String): List[Long] = {
    val parts = s.split("-").map(_.trim)

    require(
      parts.length == 2
        && parts.forall((ss) => ss.toLongOption.isDefined),
      s"Invalid input for the range $s with split ${parts(0)} - ${parts(1)}"
    )

    return parts.map(_.toLong).toList
  }

  return readFile(src_path).split(",").map(parseRange).toList
}

def getFunnyNumbersInRange(range: List[Long]): List[Long] = {

  def isFunny(number: Long): Boolean = {
    val ns = number.toString
    val digits = ns.length
    val limit = math.ceil(digits / 2.0).toInt
    val sNumbers = (1 to limit).map { i => ns.take(i) }.toList

    return sNumbers
      .map((sn) =>
        (((ns.length % sn.length) == 0)
          && ((ns.length / sn.length) == 2)
          && (ns == sn * (ns.length / sn.length)))
      )
      .contains(true)
  }

  return (range(0) to range(1)).iterator.toList.filter(isFunny).toList
}

@main
def entry() = {
  var ranges = getFileRanges(inputFilePath)

  val funny = ranges.map((r) => getFunnyNumbersInRange(r))

  // funny.foreach {
  //   (f) => println(s"Funny numbers = $f")
  // }

  print(s"Funny sum = ${funny.flatten.sum}")
}
