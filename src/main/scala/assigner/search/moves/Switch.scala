package assigner.search.moves

import assigner.model._
import assigner.search.Assignment

import org.coinor.opents._

/**
 * [[Move]] to change the [[Group]] assigned to a [[Student]].
 * @param student ID of the student to switch
 * @param group ID of the group to switch to
 */
case class Switch(student: StudentId, group: GroupId) extends Move {
  def operateOn(solution: Solution) = solution match {
    case assign: Assignment if assign.groupOf(student) != group =>
      assign.lastMove = this
      val oldGroup = assign.groupOf(student)
      val oldStudents = assign.studentsIn(oldGroup) - student
      val newStudents = assign.studentsIn(group) + student
      assign.studentMap += student -> group
      assign.groupMap += oldGroup -> oldStudents
      assign.groupMap += group -> newStudents

    case _ =>
  }

  // in case of using a TabuQueue
  override def equals(that: Any) = that match {
    case Switch(s, _) => student == s
    case _ => false
  }
}
