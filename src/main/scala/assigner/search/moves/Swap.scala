package assigner.search.moves

import assigner.model._
import assigner.search._

import org.coinor.opents._

/**
 * [[Move]] to swap the [[Group]]s of two [[Student]]s.
 * @param student1 first student to swap
 * @param student2 second student to swap
 */
case class Swap(student1: Long, student2: Long) extends Move {
  def operateOn(solution: Solution) = solution match {
    case assignment: Assignment =>
      val group1 = assignment groupOf student1
      val group2 = assignment groupOf student2
      val newStudents1 = assignment.studentsIn(group1) - student1 + student2
      val newStudents2 = assignment.studentsIn(group2) + student1 - student2
      assignment.studentMap += student1 -> group2
      assignment.studentMap += student2 -> group1
      assignment.groupMap   += group1   -> newStudents1
      assignment.groupMap   += group2   -> newStudents2
      assignment.lastMove    = this

    case _ =>
  }

  // in case of using a TabuQueue
  override def equals(that: Any) = that match {
    case Swap(s1, s2) => Set(student1, student2) == Set(s1, s2)
    case _ => false
  }
}
