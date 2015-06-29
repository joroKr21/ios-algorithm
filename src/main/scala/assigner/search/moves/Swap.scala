package assigner.search.moves

import assigner.model._
import assigner.search.Assignment

import org.coinor.opents._

/**
 * [[Move]] to swap the [[Group]]s of two [[Student]]s.
 * @param student1 first student to swap
 * @param student2 second student to swap
 */
case class Swap(student1: StudentId, student2: StudentId) extends Move {
  def operateOn(solution: Solution) = solution match {
    case assign: Assignment =>
      assign.lastMove = this
      val group1 = assign.groupOf(student1)
      val group2 = assign.groupOf(student2)
      val newStudents1 = assign.studentsIn(group1) - student1 + student2
      val newStudents2 = assign.studentsIn(group2) + student1 - student2
      assign.studentMap += student1 -> group2
      assign.studentMap += student2 -> group1
      assign.groupMap += group1 -> newStudents1
      assign.groupMap += group2 -> newStudents2

    case _ =>
  }

  // in case of using a TabuQueue
  override def equals(that: Any) = that match {
    case Swap(s1, s2) => Set(student1, student2) == Set(s1, s2)
    case _ => false
  }
}
