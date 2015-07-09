package assigner.search

import assigner.model._

/**
 * Recursive brute force solver. The recursion depth is proportional to the
 * number of groups in the `course`.
 */
class BruteForce(course: Course) {
  private val students  = course.studentMap
  private val groups    = course.groupMap
          val objective = new Objective(course)

  /** Recursive function to generate all possible [[Assignment]]s. */
  def assign(
      students: Map[StudentId, Student] = students,
      groups:   Map[GroupId,   Group]   = groups): Stream[Assignment] =
    if (groups.isEmpty || students.size < groups.values.map { _.minSize }.min) {
      if (students.values exists { _.mandatory }) Stream.empty else {
        val studentMap = students mapValues { _ => -1l }
        val groupMap   = groups   mapValues { _ => Set.empty[StudentId] }
        val queue      = default.queueId -> students.keySet
        Assignment(studentMap, groupMap + queue) #:: Stream.empty
      }
    } else {
      for {
        (g, group) <- groups.toStream
        size       <- group.minSize to (group.maxSize min students.size)
        selection  <- students.keySet subsets size
        assignment <- assign(students -- selection, groups - g)
        studentMap  = assignment.studentMap ++ selection.map { _ -> g }
        groupMap    = assignment.groupMap    + (g -> selection)
      } yield Assignment(studentMap, groupMap)
    }.distinct

  def solution = assign() maxBy objective.score
}
