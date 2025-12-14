import scala.io.Source
import scala.util.{Try, Success, Failure}

val inputFilePath: String = "./day_3/input"

def getFileBatteries(src_path: String): List[String] = {

  def readFile(path: String): List[String] = {
    val source = Source.fromFile(path)
    return try { source.getLines().toList }
    finally { source.close() }
  }

  def parseBattery(bat: String): String = {
    require(
      bat.split("").forall((s) => s.toIntOption.isDefined),
      s"Invalid input for the battery $bat!"
    )
    return bat
  }

  return readFile(src_path).map(parseBattery).toList
}

def getBatteryMaxJoltage(bat: String): Int = {
  val digits = bat.split("").filter(_ != "")

  return if (digits.length < 2) { 0 }
  else {
    (0 until digits.length - 1)
      .flatMap(i => (i + 1 until digits.length).map(j => (digits(i) + digits(j)).toInt))
      .max
  }
}

@main
def entry() = {
  var batteries = getFileBatteries(inputFilePath)

  val joltages = batteries.map(getBatteryMaxJoltage)

  // batteries.zip(joltages).foreach { (b, j) =>
  //   println(s"Battery = $b with joltage $j")
  // }

  print(s"Joltage sum = ${joltages.sum}")
}
