object Main extends App {

  import scala.io.Source
  import scala.annotation.tailrec

  val filename = "input1.txt"
  val input = Source.fromFile(filename).getLines.toList

  // part 1
  println(input.map(_.toInt).sum)

  // part 2
  def sumTwice(ls: List[String]): Int = {
    @tailrec def sumTwiceAcc(xs: List[String], acc: Int, scores: List[Int]): Int = {
      //println(acc)
      //println(scores)
      xs match {
        case Nil => sumTwiceAcc(ls, acc, scores)
        case head :: tail => {
          if (scores.contains(head.toInt + acc)) head.toInt + acc
          else  sumTwiceAcc(tail, head.toInt + acc, head.toInt + acc :: scores)
        }
      }
    }
    sumTwiceAcc(ls, 0, List(0))
  }
  println(sumTwice(input))
}