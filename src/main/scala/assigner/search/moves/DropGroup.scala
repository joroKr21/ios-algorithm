package assigner.search.moves

import assigner.model._
import assigner.search._

import org.coinor.opents._

/**
 * [[Move]] to drop a whole [[Group]] to the waiting list.
 * @param group ID of the group tp be cancelled
 */
case class DropGroup(group: GroupId) extends Move {
  def operateOn(solution: Solution) = solution match {
    case assignment: Assignment =>
      for (s <- assignment studentsIn group)
        assignment.studentMap += s -> default.queueId

      assignment.groupMap += group -> Set.empty
      assignment.lastMove  = this

    case _ =>
  }
}
