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
  def hasAdjecentSymbol(mat: Array[String], y:Int, s:Int, e:Int, charSet: Set[Char]) : Boolean = {
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
    ).flatten.exists(c => charSet.contains(c))
  }

  def getAdjecentSymbolPositions(mat: Array[String], y:Int, s:Int, e:Int, charSet: Set[Char]) : List[(Int, Int)] = {
    var toTest: List[Char] = Nil
    Iterator.range(s - 1, e + 1).flatMap(
      x =>
        Iterator.range(-1, 2).map(
          dy => {
            var yy = dy + y
            val isIn = yy >= 0 && yy < mat.length && x >= 0 && x < mat(0).length
            if isIn then List((x, yy, mat(yy).charAt(x))) else List()
          }
        )
    ).flatten.filter((x, y, c) => charSet.contains(c)).map((x, y, z) => (x, y)).toList
  }

  def run1() : Unit = {
    val source = Source.fromFile("E:\\programmering\\Scala\\adventofcode\\AoC23\\src\\inputs\\day3_1.txt")
    val mat = source.getLines().toArray
    validSymbols = getAllSymbols(mat)
    val res = mat.zipWithIndex.flatMap(
      (line:String, y) => {
        var numbers = "[0-9][0-9]*".r.findAllMatchIn(line)
        var numsTups = numbers.map(b => (b.start, b.end)).filter((s, e) => hasAdjecentSymbol(mat, y, s, e, validSymbols))
        var nums = numsTups.map((s, e) => line.substring(s, e).toInt)
        nums.toList
      }
    ).toList
    println(s"Total sum day 3: ${res.sum}")
  }

  def run2() : Unit = {
    val source = Source.fromFile("E:\\programmering\\Scala\\adventofcode\\AoC23\\src\\inputs\\day3_1.txt")
    val mat = source.getLines().toArray
    val allStars:List[(Int, Int)] = mat.zipWithIndex.flatMap(
      (line, y) => line.zipWithIndex.filter((c, x) => c=='*').map((c, x) => (x, y)).toList
    ).toList
    println(allStars)
    val symSet = Set[Char]('*')
    val res:Map[(Int, Int, Int), (List[(Int, Int)])] = mat.zipWithIndex.flatMap(
      (line: String, y) => {
        var numbers = "[0-9][0-9]*".r.findAllMatchIn(line)
        var numStarMap = numbers.map(b => (b.start, b.end)).map((s, e) =>
          ((y, s, e), getAdjecentSymbolPositions(mat, y, s, e, symSet)))

        numStarMap.toList
      }
    ).toMap
    println(res)
    var stars : List[List[(Int, Int, Int)]] = allStars.map((x, y) => {
      var allAroundStar = res.filter((_, l) => l.contains((x, y)))
      // Scala compiler does not like 3-tuples, that is why tup3 is not using pattern matching!
      if allAroundStar.size==2 then allAroundStar.map((tup3, l) => (tup3._1, tup3._2, tup3._3)).toList else Nil
    }
    )
    println(stars)
    var sum = stars.filter(l => l!=Nil).map(lst => lst.map((y, s, e) => mat(y).substring(s, e).toInt).product).sum

    println(s"Run 2 total gear sum: $sum")
  }
}
