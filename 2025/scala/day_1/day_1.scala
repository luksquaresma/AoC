import scala.io.Source
import scala.util.{Try, Success, Failure}

val inputFilePath: String = "./day_1/input"
val startingPosition: Int = 50
val validInputDirections: Set[String] = Set("L", "l", "R", "r")


enum Direction:
  case Left, Right

object Direction {
  def fromChar(c: Char): Direction = c match {
    case 'L' | 'l' => Left
    case 'R' | 'r' => Right
    case _ => throw new IllegalArgumentException(s"Invalid direction: $c")
  }
}

type Movement = (Direction, Int)

def getFileMovements(src_path: String): List[Movement] = {
  
  def readFile(path: String): List[String] = {
    val source = Source.fromFile(path)
    return try {source.getLines().toList} finally {source.close()}
  }

  def parseMovement(s:String): Movement = {
    require(
      validInputDirections.contains(s(0).toString) && s.drop(1).toIntOption.isDefined,
      s"Invalid input for the movement $s"
    )
    
    return (Direction.fromChar(s(0)), s.drop(1).toInt)
  }

  return readFile(src_path).map(parseMovement)
}


def getClicks(movementList: List[Movement]): List[Int] = {
  return movementList.map(
    (dir, num) => dir match {
      case Direction.Left => -1 * num
      case Direction.Right => num
    }
  )
}

def getZeroPasses(positions: List[Int], clicks: List[Int]): List[Int] = {
  positions.dropRight(1).zip(positions.tail).zip(clicks).map {
    case ((startPos, endPos), click) =>
      val n = startPos + click
      val passes = (if (n >= 0) n else 99 - n) / 100

      if (endPos == 0 && passes > 0) passes - 1 else passes
  }
} 


def getPosition(fromPosition: Int, clicks: Int): Int = {
  return (((fromPosition + clicks) % 100) + 100) % 100
}


@main
def entry() = {
  var instructions = getFileMovements(inputFilePath)
  val clicks = getClicks(instructions)
  
  var positions = List(startingPosition)
  for (c <- clicks) {
    positions = positions :+ getPosition(positions.last, c)
  }

  val positionsOnZero = positions.map((p) => if (p == 0) then 1 else 0)
  val passesOnZero = getZeroPasses(positions, clicks)

  positions.dropRight(1).zip(positions.tail).zip(instructions).zip(passesOnZero).foreach {
    case (((startPos, endPos), instruction), passes) =>
      println(s"Position $startPos -> $endPos, \tinstruction $instruction, \tcrossed zero $passes time(s)")
  }

  println(s"\nTotal stops on position zero == ${positionsOnZero.sum}")
  println(s"Total passes on position zero == ${passesOnZero.sum}")
  println(s"Total passes + stops on position zero == ${positionsOnZero.sum + passesOnZero.sum}")
}

