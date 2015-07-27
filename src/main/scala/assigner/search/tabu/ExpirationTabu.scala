package assigner.search.tabu

import assigner.model._
import assigner.search.moves._

import org.coinor.opents._

import scala.collection.mutable

/**
 * This implementation of [[TabuList]] will keep per-student and per-group
 * timestamps of their last move and will prohibit moves that involve students
 * / groups that were moved in the last `n` moves.
 * @param course the course to optimize
 */
class ExpirationTabu(course: Course) extends TabuList {
  val delta       = course.settings.tabuSize
  var currentTime = 0
  val studentTime = mutable.Map.empty[Long, Int] withDefaultValue 0
  val groupTime   = mutable.Map.empty[Long,   Int] withDefaultValue 0

  def setTabu(solution: Solution, move: Move) = {
    move match {
      case Switch(s, _)    => updateStudents(s)
      case Swap(s1, s2)    => updateStudents(s1, s2)
      case DropGroup(g)    => updateGroups(g)
      case FillGroup(g, _) => updateGroups(g)
    }

    currentTime += 1
  }

  def isTabu(solution: Solution, move: Move) = move match {
    case Switch(s, _)    => testStudents(s)
    case Swap(s1, s2)    => testStudents(s1, s2)
    case DropGroup(g)    => testGroups(g)
    case FillGroup(g, _) => testGroups(g)
  }

  private def updateStudents(ids: Long*) =
    for (s <- ids) studentTime(s) = currentTime

  private def testStudents(ids: Long*) =
    ids forall { currentTime - studentTime(_) < delta }

  private def updateGroups(ids: Long*) =
    for (g <- ids) groupTime(g) = currentTime

  private def testGroups(ids: Long*) =
    ids forall { currentTime - groupTime(_) < delta }
}
