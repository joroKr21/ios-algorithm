package assigner.search

import assigner._
import assigner.model._
import assigner.search.moves._

/** Random starting point generator. */
class StartingPoint(course: Course) extends Assignment(Map.empty, Map.empty) {
  init()

  private def init() = {
    // initially put everyone in the waiting list
    studentMap = course.studentMap mapValues { _ => default.queueId }
    groupMap   = course.groupMap mapValues { _ => Set.empty[StudentId] }
    groupMap   = groupMap.updated(default.queueId, Set.empty)
    groupMap   = groupMap merge studentMap.reversed

    val manager    = new Manager(course)
    val assignment = Assignment(studentMap, groupMap)

    for {
      _ <- 1 to course.settings.initialMoves
      moves  = manager.getAllMoves(assignment).toList.shuffle
      refill = moves find { _.isInstanceOf[FillGroup] }
    } refill match {
      case Some(r) => r.operateOn(assignment)
      case None    => moves.head.operateOn(assignment)
    }

    studentMap = assignment.studentMap
    groupMap   = assignment.groupMap
  }
}
