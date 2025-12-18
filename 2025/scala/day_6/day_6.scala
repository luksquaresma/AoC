import scala.io.Source
import scala.util.{Try, Success, Failure}

val inputFilePath: String = "./day_6/input"

type Operation = (String, List[Long])

def getFileOperations(src_path: String): List[Operation] = {

  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    return try { source.mkString }
    finally { source.close() }
  }

  def parseLine(s: String): List[String] = {
    return s.trim.split(" ").filter(_.nonEmpty).map(_.trim()).toList
  }

  def parseOperation(ls: List[String]): Operation = {
    require(
      (ls.last == "+") | (ls.last == "*"),
      s"Wrong operation type! \n  Invalid input for operation $ls!"
    )

    require(
      ls.dropRight(1).forall(_.toLongOption.isDefined),
      s"Wrong datatype for operation data! \n  Invalid input for operation $ls!"
    )

    return (ls.last, ls.dropRight(1).map(_.toLong))
  }

  return readFile(src_path).split("\n").map(parseLine(_)).toList.transpose.map(parseOperation(_))
}

def calculateOperation(op: Operation): Long = op(0) match {
  case "+" => op(1).sum
  case "*" => op(1).product
  case _   => throw new IllegalArgumentException(s"Invalid operation type: ${op(0)}")
}

@main
def entry() = {
  var operations = getFileOperations(inputFilePath)

  var results = operations.map(calculateOperation)

  println(s"The sum of operations is ${results.sum}")
}
