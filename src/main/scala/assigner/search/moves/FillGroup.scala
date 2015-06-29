package assigner.search.moves

import assigner.model._
import assigner.search.Assignment

import org.coinor.opents._

/**
 * [[Move]] to refill an empty [[Group]] from the waiting list.
 * @param group ID of the group to be filled
 */
case class FillGroup(group: GroupId, students: Set[StudentId]) extends Move {
  def operateOn(solution: Solution) = solution match {
    case assign: Assignment if (students &~ assign.queue.toSet).isEmpty =>
      assign.lastMove = this
      assign.groupMap += group -> students
      for (s <- students) assign.studentMap += s -> group

    case _ =>
  }

  // in case of using a TabuQueue
  override def equals(that: Any) = that match {
    case FillGroup(g, _) => group == g
    case _ => false
  }
}
