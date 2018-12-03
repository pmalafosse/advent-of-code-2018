object Main extends App {

  import scala.io.Source
  import scala.annotation.tailrec

  val filename2 = "input2.txt"
  val input2 = Source.fromFile(filename2).getLines.toList

  // part 1
  def countLetters(s: String): (Int, Int) = {
    val letters = s.distinct.toList
    val count = letters.map(x => s.count(_ == x))
    if (count.contains(2) && count.contains(3)) (1, 1)
    else if (count.contains(2)) (1, 0)
    else if (count.contains(3)) (0, 1)
    else (0, 0)
    }

  val finalCounts = input2.map(countLetters(_)).foldLeft((0, 0)) { case ((acc1, acc2), (a, b)) => (acc1 + a, acc2 + b)}
  println(finalCounts._1 * finalCounts._2)

  // part 2
  def compareStr(s1: String, s2: String): Boolean = {
    @tailrec def compareStrAcc(xs1: String, xs2: String, diffs: Int): Boolean = {
      xs1.toList match {
        case Nil => true
        case head :: _ if(head == xs2.head) => compareStrAcc(xs1.tail, xs2.tail, diffs)
        case _ if (diffs == 1) => false
        case _ => compareStrAcc(xs1.tail, xs2.tail, diffs + 1)
      }
    }
    compareStrAcc(s1, s2, 0)
  }

  def findComm(ls: List[String]): String = {
    @ tailrec def findComm_(x: String, xs: List[String], xsOr: List[String]): (String, String) = {
      if (xs.isEmpty) findComm_(xsOr.head, xsOr.tail, xsOr.tail)
      else {
        if (compareStr(x, xs.head)) (x, xs.head)
        else findComm_(x, xs.tail, xsOr)
      }
    }

    def getCommon(s1: String, s2: String): String = {
      @tailrec def getCommonAcc(xs1: String, xs2: String, common: List[Char]): String = {
        xs1.toList match {
          case Nil => common.mkString
          case head :: _ if(head == xs2.head) => getCommonAcc(xs1.tail, xs2.tail, common ::: List(head))
          case _ => getCommonAcc(xs1.tail, xs2.tail, common)
        }
      }
      getCommonAcc(s1, s2, List())
    }

    val commons = findComm_(ls.head, ls.tail, ls.tail.tail)
    getCommon(commons._1, commons._2)
  }
  println(findComm(input2))
}