package assigner.search.moves

import assigner.model._
import assigner.search.Assignment

import org.coinor.opents._

/**
 * [[Move]] to drop a whole [[Group]] to the waiting list.
 * @param group ID of the group tp be cancelled
 */
case class DropGroup(group: GroupId) extends Move {
  def operateOn(solution: Solution) = solution match {
    case assign: Assignment =>
      assign.lastMove = this
      for (s <- assign.studentsIn(group))
        assign.studentMap += s -> default.queueId

      assign.groupMap += group -> Set.empty

    case _ =>
  }
}
