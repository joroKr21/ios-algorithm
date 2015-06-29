package assigner.search

import assigner.model._
import assigner.search.moves._

import org.coinor.opents._

/** Generator of all possible moves from a given assignment. */
class Manager(course: Course) extends MoveManager {
  val students = course.studentMap
  val groups = course.groupMap

  def getAllMoves(solution: Solution) = solution match {
    case assign: Assignment =>
      val swaps = for {
        g1 #:: g2 #:: _ <- assign.groupMap.keys.toStream.combinations(2)
        s1 <- assign.studentsIn(g1)
        s2 <- assign.studentsIn(g2)
      } yield Swap(s1, s2)

      val switches = for {
        g1 <- assign.groupMap.keys
        g2 <- assign.trueGroups.keys
        if g1 != g2
        students1 = assign.studentsIn(g1)
        students2 = assign.studentsIn(g2)
        if students1.size > groups(g1).minSize
        if students2.size < groups(g2).maxSize
        s <- students1
      } yield Switch(s, g2)

      val refills = for {
        g <- assign.groupMap.filter(_._2.isEmpty).keys
        n = groups(g).minSize
        q = assign.queue
        if n >= q.size
        ss <- q.combinations(n)
      } yield FillGroup(g, ss.toSet)

      val drops = if (course.dropGroups) {
        for (g <- assign.trueGroups.keys) yield DropGroup(g)
      } else Nil

      (swaps ++ switches ++ refills ++ drops).toArray
  }
}