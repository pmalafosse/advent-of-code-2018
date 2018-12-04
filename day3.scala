object Main extends App {
  import scala.io.Source
  import scala.annotation.tailrec

  val filename = "input3.txt"
  val lines = Source.fromFile(filename).getLines.toList

  // Part 1
  val pattern = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r

  def extractValues(s: String): (Int, Int, Int, Int) = {
    val pattern(_, x1, y1, n, m) = s
    (x1.toInt, y1.toInt, n.toInt, m.toInt)
  }

  def createSet(x: Int, y: Int, n: Int, m: Int): Set[(Int, Int)] = {
    val vals = for {
      i <- x to x + n - 1
      j <- y to y + m - 1
    } yield (i, j)
    vals.toSet
  }

  def countDoubles(ls: List[String]): Int = {
    @tailrec def countDoublesAcc(xs: List[String], acc: Set[(Int, Int)], doubles: Set[(Int, Int)]): Set[(Int, Int)] = {
      xs match {
        case Nil => doubles
        case head :: tail => {
          val values = extractValues(head)
          val newSet = createSet(values._1, values._2, values._3, values._4)
          countDoublesAcc(tail, acc union newSet, doubles union (newSet intersect acc))
        }
      }
    }
    countDoublesAcc(ls, Set(), Set()).size
  }
  // println(createSet(1,1,3,3))
  println(countDoubles(lines))

  // part 2
  @tailrec def commonArea(xs: String, ls: List[String]): Boolean = {
    ls match {
      case Nil => false
      case head :: tail if (createSet(extractValues(head)._1, extractValues(head)._2, extractValues(head)._3, extractValues(head)._4) intersect createSet(extractValues(xs)._1, extractValues(xs)._2, extractValues(xs)._3, extractValues(xs)._4)).isEmpty
        => commonArea(xs, tail)
      case _ => true
    }
  }

  def findSingle(ls: List[String]): Int = {
    @tailrec def findSingleAcc(xs: List[String], acc: Int): Int = {
      xs match {
        case Nil => throw new Exception("no match")
        case head :: tail if commonArea(head, tail) => findSingleAcc(tail, acc + 1)
        case _ => acc
        }
      }
    findSingleAcc(ls, 1)
  }
  println(findSingle(lines))
}
