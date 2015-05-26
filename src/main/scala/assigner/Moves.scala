package assigner

import org.coinor.opents.{Solution, Move}

case class Swap(studentId1: Int, studentId2: Int) extends Move {
  override def operateOn(solution: Solution): Unit = solution match {
    case solution: Assignment =>
      val groupId1 = solution.studentMap(studentId1)
      val groupId2 = solution.studentMap(studentId2)
      solution.studentMap += studentId1 -> groupId2
      solution.studentMap += studentId2 -> groupId1
      solution.groupMap += groupId1 -> (solution.groupMap(groupId1) - studentId1 + studentId2)
      solution.groupMap += groupId2 -> (solution.groupMap(groupId2) + studentId1 - studentId2)
  }
}

case class Change(studentId1: Int, groupId: Int) extends Move {
  override def operateOn(solution: Solution): Unit = solution match {
    case solution: Assignment =>
      solution.studentMap += studentId1 -> groupId
      val old = solution.studentMap(studentId1)
      solution.groupMap += groupId -> (solution.groupMap(groupId) + studentId1)
      solution.groupMap += old -> (solution.groupMap(old) - studentId1)
  }
}