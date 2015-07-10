package assigner.search

import assigner._
import assigner.model._

import scala.collection.SortedSet

/** Random starting point generator. */
object StartingPoint {
  def apply(course: Course): Assignment = {
    val studentMap =
      course.studentMap.sorted mapValues { _ => default.queueId }

    val groupMap =
      course.groupMap.sorted mapValues { _ => SortedSet.empty[StudentId] }

    val queue      = default.queueId -> studentMap.keySet
    val manager    = new Manager(course)
    val assignment = Assignment(studentMap, groupMap + queue)

    for (_ <- 1 to course.settings.initialMoves) {
      manager.getAllMoves(assignment).toList.shuffle match {
        case move :: _ => move.operateOn(assignment)
        case Nil =>
      }
    }

//    val students = course.studentMap partition { _._2.mandatory } match {
//      case (mandatory, elective) =>
//        mandatory.toList.shuffle ::: elective.toList.shuffle
//    }
//
//    val groups = course.groupMap partition { _._2.mandatory } match {
//      case (mandatory, elective) =>
//        mandatory.toList.shuffle ::: elective.toList.shuffle
//    }

    assignment
  }
}
