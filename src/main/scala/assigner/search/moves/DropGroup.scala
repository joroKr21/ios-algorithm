package assigner.search.moves

import assigner.model._
import assigner.search._

import org.coinor.opents._

import scala.collection.SortedSet

/**
 * [[Move]] to drop a whole [[Group]] to the waiting list.
 * @param group ID of the group tp be cancelled
 */
case class DropGroup(group: GroupId) extends Move {
  def operateOn(solution: Solution) = solution match {
    case assignment: Assignment =>
      val queueId  = default.queueId
      val queue    = assignment.queue
      val students = assignment studentsIn group
      for (s <- students) assignment.studentMap += s -> queueId
      assignment.groupMap += queueId -> (queue | students)
      assignment.groupMap += group   -> SortedSet.empty
      assignment.lastMove  = this

    case _ =>
  }
}
