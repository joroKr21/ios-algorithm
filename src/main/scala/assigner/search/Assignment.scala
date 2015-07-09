package assigner.search

import assigner.model._

import org.coinor.opents._

/**
 * An assignment of students to groups that represents a valid solution.
 * @param studentMap [[Map]] of [[StudentId]] -> [[GroupId]]
 * @param groupMap [[Map]] of [[GroupId]] to students in that group
 */
case class Assignment(
      var studentMap: Map[StudentId, GroupId],
      var groupMap:   Map[GroupId, Set[StudentId]])
    extends SolutionAdapter {

  var lastMove: Move = _

  /** @return the group assigned to `student`.*/
  def groupOf(student: StudentId): GroupId = studentMap(student)

  /** @return the [[Set]] of students assigned to `group`. */
  def studentsIn(group: GroupId): Set[StudentId] = groupMap(group)

  /** @return `groupMap` without empty groups and the waiting list */
  def trueGroups: Map[GroupId, Set[StudentId]] =
    groupMap filterNot { case (g, ss) => g.isQueue || ss.isEmpty }

  /** @return a [[Set]] of all [[Student]]s currently in the waiting list */
  def queue: Set[StudentId] = studentsIn(default.queueId)

  /**
   * Extract the IDs of all [[Student]]s currently in the waiting list.
   * @param o implicit ordering scheme for the students
   * @return an ordered list of students in the queue
   */
  def waitingList(implicit o: Ordering[StudentId]): List[StudentId] =
    queue.toList sorted o

  override def clone = {
    val copy = super.clone.asInstanceOf[Assignment]
    copy.studentMap = studentMap
    copy.groupMap   = groupMap
    copy
  }
}
