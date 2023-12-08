import scala.io.Source
object Day2 {
  val maxValues = Map[String, Int](
    "red" -> 12,
  "green" -> 13,
  "blue" -> 14
  )

  val colors = List("red", "green", "blue")

  def run1() : Unit = {
    val source = Source.fromFile("E:\\programmering\\Scala\\adventofcode\\AoC23\\src\\main\\scala\\day2_1.txt")

    //(id, Map[])
    val games = source.getLines().map(
      l =>
      {
        val sideSplit = l.split(":")
        val id = sideSplit.head.split(" ").last.toInt
        val sets = sideSplit.last.split(";")
        val maps = sets.map(
          s => {
            val colorPairs = s.split(", ")
            colorPairs.map(cp => {
              val parts = cp.split(" ").filter(s => s.trim().nonEmpty)
              (parts.last, parts.head.toInt)
            }
            ).toMap
          }
        )
        (id, maps)
      }
    ).toList

    val sum = games.filter(m => !m._2.toList.exists(p => p.exists((s, i) => maxValues(s)<i))).map(g => g._1).sum
    println(s"Total sum = $sum");
  }

  def run2() : Unit = {
    val source = Source.fromFile("E:\\programmering\\Scala\\adventofcode\\AoC23\\src\\main\\scala\\day2_1.txt")

    //(id, Map[])
    val games = source.getLines().map(
      l => {
        val sideSplit = l.split(":")
        val id = sideSplit.head.split(" ").last.toInt
        val sets = sideSplit.last.split(";")
        val maps = sets.map(
          s => {
            val colorPairs = s.split(", ")
            colorPairs.map(cp => {
              val parts = cp.split(" ").filter(s => s.trim().nonEmpty)
              (parts.last, parts.head.toInt)
            }
            ).toMap
          }
        )
        (id, maps)
      }
    ).toList

    val minimums = games.map(
      g => {
        var min = colors.map(c => (c, 0)).toMap
        g._2.reduce((b, a) => colors.map(c => (c, Math.max(a.getOrElse(c, 1), b.getOrElse(c, 1)))).toMap)
      }
    )

    val sum = minimums.map(m => m.toList.map(v => v._2).reduce((a, b) => a*b)).sum

    println(s"Total sum 2 = $sum");
  }

}
