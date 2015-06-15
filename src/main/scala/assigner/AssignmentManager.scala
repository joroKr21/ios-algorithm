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
              val toPrefIndexStudent1 = students(studentId1).groupPreferences.indexOf(groupOfStudent2)
              val fromPrefIndexStudent1 = students(studentId1).groupPreferences.indexOf(groupOfStudent1)
              val toPrefIndexStudent2 = students(studentId2).groupPreferences.indexOf(groupOfStudent1)
              val fromPrefIndexStudent2 = students(studentId2).groupPreferences.indexOf(groupOfStudent2)

              // Check if the new matching is stable
              !(fromPrefIndexStudent1 > toPrefIndexStudent1 && fromPrefIndexStudent2 > toPrefIndexStudent2)
            }
          case Change(studentId, groupId) =>
            val from = solution.studentMap(studentId)
            val toPrefIndex = students(studentId).groupPreferences.indexOf(groupId)
            val fromPrefIndex = students(studentId).groupPreferences.indexOf(from)

            // Check if the new matching is stable
            toPrefIndex >= fromPrefIndex
        }



        val swaps = (for {
          groupId1 #:: groupId2 #:: _ <- groups.keys.toStream.combinations(2)
          studentId1 <- solution.groupMap(groupId1)
          studentId2 <- solution.groupMap(groupId2)
          move = Swap(studentId1, studentId2)
          if isStable(move)
        } yield move).toVector

        val changes = (for {
          groupId1 <- groups.keys
          groupId2 <- groups.keys
          if groupId2 != -1
          if groupId1 != groupId2
          if solution.groupMap(groupId1).size > groups(groupId1).minSize
          if solution.groupMap(groupId2).size < groups(groupId2).maxSize
          studentId <- solution.groupMap(groupId1)
        } yield Change(studentId, groupId2)).toVector

        (swaps ++ changes).toArray
      }
    }
  }
}