package assigner.search.moves

import assigner.model._
import assigner.search._

import org.coinor.opents._

/**
 * [[Move]] to change the [[Group]] assigned to a [[Student]].
 * @param student ID of the student to switch
 * @param group   ID of the group to switch to
 */
case class Switch(student: Long, group: Long) extends Move {
  def operateOn(solution: Solution) = solution match {
    case assignment: Assignment if assignment.groupOf(student) != group =>
      val oldGroup    = assignment groupOf student
      val oldStudents = assignment.studentsIn(oldGroup) - student
      val newStudents = assignment.studentsIn(group)    + student
      assignment.studentMap += student  -> group
      assignment.groupMap   += oldGroup -> oldStudents
      assignment.groupMap   += group    -> newStudents
      assignment.lastMove    = this

    case _ =>
  }

  // in case of using a TabuQueue
  override def equals(that: Any) = that match {
    case Switch(s, _) => student == s
    case _ => false
  }
}