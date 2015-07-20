package assigner.search

import assigner.model._

import org.coinor.opents._

import scala.collection.{SortedSet, SortedMap}

/**
 * An assignment of students to groups that represents a valid solution.
 * @param studentMap [[Map]] of [[Long]] -> [[Long]]
 * @param groupMap [[Map]] of [[Long]] to students in that group
 */
case class Assignment(
      var studentMap: SortedMap[Long, Long],
      var groupMap:   SortedMap[Long, SortedSet[Long]])
    extends SolutionAdapter {

  var lastMove: Move = _

  /** @return the group assigned to `student`.*/
  def groupOf(student: Long): Long = studentMap(student)

  /** @return the [[Set]] of students assigned to `group`. */
  def studentsIn(group: Long): SortedSet[Long] = groupMap(group)

  /** @return `groupMap` without empty groups and the waiting list */
  def trueGroups: SortedMap[Long, SortedSet[Long]] =
    groupMap filterNot { case (g, ss) => g.isQueue || ss.isEmpty }

  /** @return a [[Set]] of all [[Student]]s currently in the waiting list */
  def queue: SortedSet[Long] = studentsIn(default.queueId)

  /**
   * Extract the IDs of all [[Student]]s currently in the waiting list.
   * @param o implicit ordering scheme for the students
   * @return an ordered list of students in the queue
   */
  def waitingList(implicit o: Ordering[Long]): List[Long] =
    queue.toList sorted o

  override def clone = {
    val copy = super.clone.asInstanceOf[Assignment]
    copy.studentMap = studentMap
    copy.groupMap   = groupMap
    copy
  }
}
