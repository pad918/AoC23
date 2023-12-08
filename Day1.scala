import scala.io.Source
  val stringMap = Map[String, Int](
    "one" -> 1,
    "two"   -> 2,
    "three" -> 3,
    "four"  -> 4,
    "five"  -> 5,
    "six"   -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine"  -> 9,
    "0"     -> 0,
    "1"     -> 1,
    "2"     -> 2,
    "3"     -> 3,
    "4"     -> 4,
    "5"     -> 5,
    "6"     -> 6,
    "7"     -> 7,
    "8"     -> 8,
    "9"     -> 9,
  )

object Day1 {
  def run1() : Unit =  {
    val source = Source.fromFile("E:\\programmering\\Scala\\adventofcode\\AoC23\\src\\main\\scala\\day1_1.txt")
    val sum = source.getLines().map(
      l =>
        l.filter(c => '0'<=c && '9'>= c)
    ).map(
      l =>
        ((l.head-'0').toInt*10) + (l.last-'0').toInt
    ).reduce((a, b) => a+b)

    println(s"Total sum = $sum")
  }

  def run2() : Unit = {
    val source = Source.fromFile("E:\\programmering\\Scala\\adventofcode\\AoC23\\src\\main\\scala\\day1_1.txt")
    val lineValues = source.getLines().map(
      l => "[0-9]|one|two|three|four|five|six|seven|eight|nine".r.findAllIn(
          // Filthy hack >=)
          l.replace("eight", "eightt")
            .replace("three", "threee")
            .replace("five", "fivee")
            .replace("two", "twoo")
            .replace("one", "onee")
        )
        .map(str => stringMap.get(str).get).toList
    ).map(l => l.head*10 + l.last)

    println(s"Sum of run2: ${lineValues.sum}")
  }
}
