object Main extends App {
  import scala.io.Source
  import scala.util.matching.Regex
  import java.util.Date
  import java.util.concurrent.TimeUnit

  val filename = "/input4.txt"
  val shifts = Source.fromFile(filename).getLines.toList

  // Part 1

  val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
  val last_extract_value="1518-05-09 10:04"
  val string_to_date = format.parse(last_extract_value)
  // println(string_to_date)

  case class Shift(date: Date, hour: Int, minute: Int, details: String)

  def extractDate(x: String): Shift = {
    val Pattern: Regex =  """\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):(\d+)\] (.*)""".r
    x match {
      case Pattern(y, month, d, h, m, details) if(h == "23") => {
        val date = format.parse(y + "-" + month + "-" + d)
        val newDate = new Date(date.getTime() + TimeUnit.DAYS.toMillis( 1 ))
        Shift(newDate, 0, 0, details)
      }
      case Pattern(y, month, d, h, m, details) => Shift(format.parse(y + "-" + month + "-" + d), h.toInt, m.toInt, details)
    }
  }

  // println(shifts.map(extractDate(_)))

  val shiftsDay = shifts.map(extractDate(_)).groupBy(_.date)
  println(shiftsDay)

  case class ShiftDay(date: Date, guard: String, awake: List[Int], asleep: List[Int])

  val ls = shiftsDay.toList

  case class dayShifts(date: Date, listShifts: List[Shift])

  val sortedls = ls.map(x => (x._1, x._2.sortBy(x => (x.hour, x.minute))))
  println(sortedls(0))
  println("************")


  def extractGuardShifts(x: (Date, List[Shift])): (Date, String, List[Shift]) = {
    val Pattern: Regex =  """Guard #([0-9]+) (.*)""".r

    def filterGuard(y: Shift): Boolean = {
      y.details match {
        case Pattern(_, _) => false
        case _ => true
      }
    }
    def extractGuard(xs: List[Shift]): String = {
      if(xs.isEmpty) throw new Exception("No Guard ID")
      xs.head.details match {
        case Pattern(id, _) => id
        case _ => extractGuard(xs.tail)
      }
    }
    val filteredList = x._2.filter(filterGuard(_))

    println(filteredList)
    (x._1, extractGuard(x._2), filteredList)

  }

  val guardsShifts = sortedls.map(extractGuardShifts(_))
  println(guardsShifts)

  def buildMinutesShift(x: (Date, String, List[Shift])): ShiftDay = {

    def buildSet(xs: List[Shift], awake: Boolean, previous_minute: Int, currSet: (List[Int], List[Int])) : (List[Int], List[Int]) = {
      xs match {
        case Nil if(previous_minute == 59) => currSet
        case Nil if previous_minute < 59 => {
          val to_add = for {
            i <- previous_minute to 59
          } yield (i)
          if(awake) (currSet._1 union to_add.toList, currSet._2)
          else (currSet._1, currSet._2 union to_add.toList)
        }
        case Shift(_, _, minute, state) :: tail => {
          val to_add = for {
            i <- previous_minute to minute - 1
          } yield (i)
          val new_state = state == "wakes up"
          if(awake) buildSet(tail, new_state, minute, (currSet._1 union to_add.toList, currSet._2))
          else buildSet(tail, new_state, minute, (currSet._1, currSet._2 union to_add.toList))
        }
      }
    }
    val sets = buildSet(x._3, true, -1, (List(), List()))
    ShiftDay(x._1, x._2, sets._1.filter(x => x != -1).sorted, sets._2.filter(x => x != -1).sorted)

  }

  val cleanls = guardsShifts.map(buildMinutesShift(_))
  println(cleanls) // contains the minutes per day, awake, asleep
  println("////")

  val sumMinutesAsleep = cleanls.map(x => (x.guard, x.asleep.size)).groupBy(x => x._1)
  // one sleeping the most
  val sleepingMostList = sumMinutesAsleep.toList.map(x => (x._1, x._2.map(_._2).sum)).sortBy(-_._2)
  val sleepingMost = sleepingMostList(0)._1
  println(sleepingMost)

  // most common sleeping minute
  val sleepSets = cleanls.filter(x => x.guard == sleepingMost)
  println(sleepSets)
  val mostSleptMin = sleepSets.map(_.asleep).map(_.toList).flatten.groupBy(identity).mapValues(_.size).maxBy(_._2)._1

  println(sleepingMost.toInt * mostSleptMin.toInt)

  // part 2
  println("*****  part2 ****")
  val sleepGuards = cleanls.map(x => (x.guard, x.asleep)).groupBy(_._1).toList
    .map(x => (x._1, x._2.map(_._2).flatten))
    .filter(x => ! x._2.isEmpty)
      .map(x => (x._1, x._2.groupBy(identity).mapValues(_.size).maxBy(_._2)))
      .sortBy(- _._2._2)
  println(sleepGuards)
  println(sleepGuards(0)._1.toInt * sleepGuards(0)._2._1.toInt)
}