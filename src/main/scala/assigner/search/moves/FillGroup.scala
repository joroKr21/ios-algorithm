package assigner.search.moves

import assigner.model._
import assigner.search._

import org.coinor.opents._

import scala.collection.SortedSet

/**
 * [[Move]] to refill an empty [[Group]] from the waiting list.
 * @param group ID of the group to be filled
 */
case class FillGroup(group: GroupId, students: SortedSet[StudentId]) extends Move {
  def operateOn(solution: Solution) = solution match {
    case assignment: Assignment =>
      val queueId = default.queueId
      val queue   = assignment.queue
      if (students subsetOf queue) {
        for (s <- students) assignment.studentMap += s -> group
        assignment.groupMap += queueId -> (queue &~ students)
        assignment.groupMap += group   -> students
        assignment.lastMove  = this
      }

    case _ =>
  }

  // in case of using a TabuQueue
  override def equals(that: Any) = that match {
    case FillGroup(g, _) => group == g
    case _ => false
  }
}
