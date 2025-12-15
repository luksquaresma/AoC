import scala.io.Source
import scala.util.{Try, Success, Failure}

val inputFilePath: String = "./day_5/input"

def getFileIngredients(src_path: String): (List[List[Long]], List[Long]) = {

  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    return try { source.mkString }
    finally { source.close() }
  }

  def parseRange(s: String): List[Long] = {
    require(
      s.contains("-")
        && s.split("-").forall(_.toLongOption.isDefined)
        && s.split("-").length == 2,
      s"Invalid input for string $s!"
    )
    return s.split("-").map(_.toLong).toList
  }

  def parseIngredient(s: String): Long = {
    require(
      !s.contains("-") && s.toLongOption.isDefined,
      s"Invalid input for string $s!"
    )
    return s.toLong
  }

  val (ranges, ingredients) = readFile(src_path).split("\n\n").toList match {
    case List(r, i) =>
      (
        r.split("\n").map(parseRange).toList,
        i.split("\n").map(parseIngredient).toList
      )
    case _ =>
      throw new IllegalArgumentException("Input must contain exactly one '\\n\\n' separator")
  }

  return (ranges, ingredients)
}

def getFreshIngredientMap(freshRanges: List[List[Long]], ingredients: List[Long]): List[Boolean] = {
  return ingredients.map(checkFreshIngredient(_, freshRanges))
}

def checkFreshIngredient(ingredient: Long, freshRanges: List[List[Long]]): Boolean = {
  return freshRanges.exists((f) => ingredient >= f(0) && ingredient <= f(1))
}

@main
def entry() = {
  var (ranges, ingredients) = getFileIngredients(inputFilePath)

  var freshIngredients = ingredients.filter(checkFreshIngredient(_, ranges))

  // println(s"\nRanges:\t ${ranges.map(_.mkString("-")).mkString(", ")}")

  // println(s"\nIngredients:\t ${ingredients.mkString(", ")}")

  // println(
  //   s"\nFresh ingredient check:\t ${getFreshIngredientMap(ranges, ingredients).mkString(", ")}"
  // )

  // println(s"\nFresh ingredients:\t ${freshIngredients.mkString(", ")}")

  println(s"\nNumber or total fresh ingredients = ${freshIngredients.size}")
}
