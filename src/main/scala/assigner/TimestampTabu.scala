package assigner

import org.coinor.opents._

import scala.collection.mutable

/**
 * This implementation of [[TabuList]] will keep per-student timestamps of
 * their last move and will prohibit moving students that were moved in the
 * last n moves.
 */
case class TimestampTabu(course: Course) extends TabuList {
  val delta = course.settings.tabuSize
  var curTime = 0
  val timestamps = mutable.Map.empty[Int, Int] withDefaultValue 0

  def setTabu(fromSolution: Solution, move: Move) = {
    move match {
      case Change(s, _) => update(s)
      case Swap(s1, s2) => update(s1, s2)
    }

    curTime += 1
  }

  def isTabu(fromSolution: Solution, move: Move) = move match {
    case Change(s, _) => test(s)
    case Swap(s1, s2) => test(s1, s2)
  }

  private def update(students: Int*) =
    students foreach { timestamps(_) = curTime }

  private def test(students: Int*) =
    students forall { curTime - timestamps(_) < delta }
}
