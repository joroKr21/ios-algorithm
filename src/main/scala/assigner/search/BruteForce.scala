package assigner.search

import assigner._
import assigner.model._

import scala.collection.{SortedSet, SortedMap}

/**
 * Recursive brute force solver. The recursion depth is proportional to the
 * number of groups in the `course`.
 */
class BruteForce(course: Course) {
  private val students  = course.studentMap.sorted
  private val groups    = course.groupMap.sorted
          val objective = new Objective(course)

  /** Recursive function to generate all possible [[Assignment]]s. */
  def assign(
      students: SortedMap[StudentId, Student] = students,
      groups:   SortedMap[GroupId,   Group]   = groups): Iterator[Assignment] =
    if (groups.isEmpty ||
        students.size < groups.values.map { _.minSize }.min) {
      if (students.values.exists { _.mandatory } ||
            groups.values.exists { _.mandatory }) Iterator.empty else {
        val studentMap = students mapValues { _ => -1l }
        val groupMap   = groups   mapValues { _ => SortedSet.empty[StudentId] }
        val queue      = default.queueId -> students.keySet
        Iterator(Assignment(studentMap, groupMap + queue))
      }
    } else {
      for {
        (g, group) <- groups.iterator
        size       <- group.minSize to (group.maxSize min students.size)
        selection  <- students.keySet subsets size
        assignment <- assign(students -- selection, groups - g)
        studentMap  = assignment.studentMap ++ selection.map { _ -> g }
        groupMap    = assignment.groupMap    + (g -> selection)
      } yield Assignment(studentMap, groupMap)
    }

  def solution = assign() maxBy objective.score
}
