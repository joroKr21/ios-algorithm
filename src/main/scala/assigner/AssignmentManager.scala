package assigner

import org.coinor.opents.{Move, Solution, MoveManager}

class AssignmentManager(course: Course) extends MoveManager {
  val students = course.studentMap
  val groups   = course.groupMap

  def getAllMoves(solution: Solution) = solution match {
    case assignment @ Assignment(studentMap, groupMap) =>
      def isStable(move: Move) = move match {
        case Swap(s1, s2) =>
          val (student1, student2) = (students(s1), students(s2))
          val (g1, g2) = (studentMap(s1), studentMap(s2))
          // check if the new matching is stable
          (g1 == -1 && !student2.mandatory) ||
          (g2 == -1 && !student1.mandatory) || {
            val pref1 = student1.preferences
            val pref2 = student2.preferences
            val from1 = pref1 indexOf g1
            val to1   = pref1 indexOf g2
            val from2 = pref2 indexOf g2
            val to2   = pref2 indexOf g1
            !(from1 < to1 && from2 < to2)
          }

        case Change(s, g2) =>
          val pref = students(s).preferences
          val g1   = studentMap(s)
          val from = pref indexOf g1
          val to   = pref indexOf g2
          // check if the new matching is stable
          from <= to
      }

      val swaps = (for {
        g1 #:: g2 #:: _ <- groups.keys.toStream combinations 2
        s1 <- groupMap(g1)
        s2 <- groupMap(g2)
        swap = Swap(s1, s2)
        if isStable(swap)
      } yield swap).toVector

      val changes = (for {
        g1 <- groups.keys
        g2 <- groups.keys
        if g2 != -1 && g1 != g2
        if groupMap(g1).size > groups(g1).minSize
        if groupMap(g2).size < groups(g2).maxSize
        s  <- groupMap(g1)
      } yield Change(s, g2)).toVector

      (swaps ++ changes).toArray
  }
}