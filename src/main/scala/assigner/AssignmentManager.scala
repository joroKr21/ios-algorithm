package assigner

import org.coinor.opents.{Move, Solution, MoveManager}

class AssignmentManager(students: Map[Int, Student], groups: Map[Int, Group]) extends MoveManager {
  def getAllMoves(solution: Solution): Array[Move] = {
    solution match {
      case solution: Assignment => {
        def isStable(move: Move) = move match {
          case Swap(studentId1, studentId2) =>
            val groupOfStudent1 = solution.studentMap(studentId1)
            val groupOfStudent2 = solution.studentMap(studentId2)
            if (groupOfStudent1 == -1 || groupOfStudent2 == -1) true
            else {
              val toPrefIndexStudent1 = students(studentId1).preferences.indexOf(groupOfStudent2)
              val fromPrefIndexStudent1 = students(studentId1).preferences.indexOf(groupOfStudent1)
              val toPrefIndexStudent2 = students(studentId2).preferences.indexOf(groupOfStudent1)
              val fromPrefIndexStudent2 = students(studentId2).preferences.indexOf(groupOfStudent2)

              // Check if the new matching is stable
              !(fromPrefIndexStudent1 > toPrefIndexStudent1 && fromPrefIndexStudent2 > toPrefIndexStudent2)
            }
          case Change(studentId, groupId) =>
            val from = solution.studentMap(studentId)
            val toPrefIndex = students(studentId).preferences.indexOf(groupId)
            val fromPrefIndex = students(studentId).preferences.indexOf(from)

            // Check if the new matching is stable
            toPrefIndex >= fromPrefIndex
        }

        val moves = (for {
          groupId1 #:: groupId2 #:: _ <- groups.keys.toStream.combinations(2)
          studentId1 <- solution.groupMap(groupId1)
          studentId2 <- solution.groupMap(groupId2)
          move = Swap(studentId1, studentId2)
          if isStable(move)
        } yield move).toList

        moves.toArray
      }
    }
  }
}