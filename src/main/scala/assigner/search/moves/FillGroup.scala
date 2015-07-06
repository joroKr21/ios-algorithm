package assigner.search.moves

import assigner.model._
import assigner.search._

import org.coinor.opents._

/**
 * [[Move]] to refill an empty [[Group]] from the waiting list.
 * @param group ID of the group to be filled
 */
case class FillGroup(group: GroupId, students: Set[StudentId]) extends Move {
  def operateOn(solution: Solution) = solution match {
    case assignment: Assignment if (students &~ assignment.queue.toSet).isEmpty =>
      assignment.groupMap += group -> students
      for (s <- students) assignment.studentMap += s -> group
      assignment.lastMove = this

    case _ =>
  }

  // in case of using a TabuQueue
  override def equals(that: Any) = that match {
    case FillGroup(g, _) => group == g
    case _ => false
  }
}
