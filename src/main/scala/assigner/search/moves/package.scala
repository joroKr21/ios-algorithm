package assigner.search

import assigner.model._

import org.coinor.opents._

/** Package for all moves. */
package object moves {

  /** Super trait for all moves. */
  sealed trait Change extends Move {
    abstract override def operateOn(solution: Solution) = {
      super.operateOn(solution)
      solution match {
        case assign: Assignment => assign.lastMove = this
        case _ =>
      }
    }
  }

  /**
   * [[Move]] to swap the [[Group]]s of two [[Student]]s.
   * @param student1 first student to swap
   * @param student2 second student to swap
   */
  case class Swap(student1: StudentId, student2: StudentId) extends Change {
    override def operateOn(solution: Solution) = solution match {
      case assign: Assignment =>
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

  /**
   * [[Move]] to change the [[Group]] assigned to a [[Student]].
   * @param student ID of the student to switch
   * @param group ID of the group to switch to
   */
  case class Switch(student: StudentId, group: GroupId) extends Change {
    override def operateOn(solution: Solution) = solution match {
      case assign: Assignment if assign.groupOf(student) != group =>
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

  /**
   * [[Move]] to drop a whole [[Group]] to the waiting list.
   * @param group ID of the group tp be cancelled
   */
  case class DropGroup(group: GroupId) extends Change {
    override def operateOn(solution: Solution) = solution match {
      case assign: Assignment =>
        for (s <- assign.studentsIn(group))
          assign.studentMap += s -> default.queueId

        assign.groupMap += group -> Set.empty

      case _ =>
    }
  }

  /**
   * [[Move]] to refill an empty [[Group]] from the waiting list.
   * @param group ID of the group to be filled
   */
  case class FillGroup(group: GroupId, students: Set[StudentId]) extends Change {
    override def operateOn(solution: Solution) = solution match {
      case assign: Assignment if (students &~ assign.queue.toSet).isEmpty =>
        assign.groupMap += group -> students
        for (s <- students) assign.studentMap += s -> group

      case _ =>
    }

    // in case of using a TabuQueue
    override def equals(that: Any) = that match {
      case FillGroup(g, _) => group == g
      case _ => false
    }
  }
}
