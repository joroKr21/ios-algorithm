package assigner.search

import assigner._
import assigner.model._

/** Random starting point generator. */
object StartingPoint {
  def apply(course: Course): Assignment = {
    val studentMap = course.studentMap mapValues { _ => default.queueId }
    val groupMap   = course.groupMap   mapValues { _ => Set.empty[StudentId] }
    val queue      = default.queueId -> studentMap.keySet
    val manager    = new Manager(course)
    val assignment = Assignment(studentMap, groupMap + queue)

    for (_ <- 1 to course.settings.initialMoves) {
      manager.getAllMoves(assignment).toList.shuffle match {
        case move :: _ => move.operateOn(assignment)
        case Nil =>
      }
    }

    assignment
  }
}
