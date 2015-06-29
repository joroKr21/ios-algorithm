package assigner.search

import assigner._
import assigner.model._

/** Random starting point generator. */
class StartingPoint(course: Course) extends Assignment(Map.empty, Map.empty) {
  // initially put everyone in the waiting list
  studentMap = course.studentMap.mapValues(_ => default.queueId)
  // TODO: generate random starting point (although it could work like this)
  val initialGroups = course.groupMap.mapValues(_ => Set.empty[StudentId])
  val withQueue = initialGroups.updated(default.queueId, Set.empty)
  groupMap = withQueue.merge(studentMap.reversed)
}


