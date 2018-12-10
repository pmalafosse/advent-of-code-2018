object Main extends App {
  import scala.io.Source
  import scala.annotation.tailrec


  val filename = "input5.txt"
  val alltext = Source.fromFile(filename).getLines.toList(0)

  // part 1

  def reduceText(ls: String): String = {
    @ tailrec def reduceTextAcc(xs: String, before: List[String]): String = {
      if(xs.isEmpty) before.mkString("").reverse
      else {
        before match {
          case Nil => reduceTextAcc(xs.tail, List(xs.head.toString))
          case a :: b if ((a.toString != xs.head.toString) && (a.toLowerCase == xs.head.toString.toLowerCase)) => {
            reduceTextAcc(xs.tail, b)
          }
          case a => reduceTextAcc(xs.tail, xs.head.toString :: a)
        }
      }
    }
    reduceTextAcc(ls, List())
  }

  val processedPolymer = reduceText(alltext)
  println(processedPolymer.size)

  // part 2
  val alphabet = ('a' to 'z').toList
  val filteredText = alphabet.map(x => reduceText(alltext.filter(_.toLower != x)).size)
  println(filteredText)
  println(filteredText.min)
