import scala.io.Source
object Day3 {
  var validSymbols: Set[Char] = Set()
  def getAllSymbols(mat: Array[String]) : Set[Char] = {
    var set = Set[Char]()
    mat.foreach(m => m.foreach(
      c => if (c>'9' || c<'0' && c!='.') then set += c
    ))
    set
  }
  def hasAdjecentSymbol(mat: Array[String], y:Int, s:Int, e:Int) : Boolean = {
    var toTest:List[Char] = Nil
    Iterator.range(s-1, e+1).flatMap(
      x =>
        Iterator.range(-1, 2).map(
          dy =>
            {
              var yy = dy + y
              val isIn = yy >= 0 && yy < mat.length && x >= 0 && x < mat(0).length
              if isIn then List(mat(yy).charAt(x)) else List()
            }
        )
    ).flatten.exists(c => validSymbols.contains(c))
  }
  def run1() : Unit = {
    val source = Source.fromFile("E:\\programmering\\Scala\\adventofcode\\AoC23\\src\\inputs\\day3_1.txt")
    val mat = source.getLines().toArray
    validSymbols = getAllSymbols(mat)
    val res = mat.zipWithIndex.flatMap(
      (line:String, y) => {
        var numbers = "[0-9][0-9]*".r.findAllMatchIn(line)
        var numsTups = numbers.map(b => (b.start, b.end)).filter((s, e) => hasAdjecentSymbol(mat, y, s, e))
        var nums = numsTups.map((s, e) => line.substring(s, e).toInt)
        nums.toList
      }
    ).toList
    println(s"Total sum day 3: ${res.sum}")
  }
}
