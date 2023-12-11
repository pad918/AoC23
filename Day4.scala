import scala.collection.mutable
import scala.io.Source

object Day4 {
  def run1() : Unit = {
    val source = Source.fromFile("E:\\programmering\\Scala\\adventofcode\\AoC23\\src\\inputs\\day4_1.txt")
    val cards = source.getLines().map(
      l =>
        {
          val tst = l.split(":")(1)
          val parts = l.split(":")(1).split("\\|")
          val toIntArr = (s:String) => s.split(" ").filter(s => s.trim().nonEmpty).map(is => is.toInt).toList
          (toIntArr(parts(0)), toIntArr(parts(1)))
        }
    ).toList
    val cardPoints = cards.map(
      (win, has) => {
        val wins = has.count(n => win.contains(n))
        if wins == 0 then 0 else Iterator.range(0, wins-1).map(_ => 2).product // wins^2
      }
    ).toList.sum

    println(s"Cards: $cardPoints")
  }

  def run2(): Unit = {
    val source = Source.fromFile("E:\\programmering\\Scala\\adventofcode\\AoC23\\src\\inputs\\day4_1.txt")
    val cards = source.getLines().map(
      l => {

        val num = l.split(":")(0).split(" ").last.toInt
        val tst = l.split(":")(1)
        val parts = l.split(":")(1).split("\\|")
        val toIntArr = (s: String) => s.split(" ").filter(s => s.trim().nonEmpty).map(is => is.toInt).toList
        (num, toIntArr(parts(0)), toIntArr(parts(1)))
      }
    ).toList
    val winMap = cards.map(
      (n, win, has) => {
        val wins = has.count(n => win.contains(n))
        (n, wins)
      }
    ).toMap
    var numCopies : mutable.Map[Int, Int] = mutable.Map[Int, Int]()
    numCopies.addAll(Iterator.range(1, cards.length+1).map(i => (i, 1)))
    Iterator.range(1, cards.length+1).foreach(
      i => {
        Iterator.range(i+1, i+winMap(i)+1).foreach(
          j => numCopies.update(j, numCopies(j)+numCopies(i))
        )
      }
    )
    val totalCards = numCopies.map((a, b) => b).sum

    println(s"Total cards found = ${totalCards}")
  }
}
