package assigner.search

import assigner.model._
import assigner.search.moves._

import org.coinor.opents._

/** Generator of all possible moves from a given assignment. */
class Manager(course: Course) extends MoveManager {
  val students = course.studentMap
  val groups   = course.groupMap

  def getAllMoves(solution: Solution) = solution match {
    case assignment: Assignment =>
      val swaps = for {
        g1 #:: g2 #:: _ <- assignment.groupMap.keys.toStream combinations 2
        s1 <- assignment studentsIn g1
        s2 <- assignment studentsIn g2
      } yield Swap(s1, s2)

      val switches = for {
        g1 <- assignment.groupMap.keys
        g2 <- assignment.trueGroups.keys
        if g1 != g2
        students1 = assignment studentsIn g1
        if students1.size > groups(g1).minSize
        students2 = assignment studentsIn g2
        if students2.size < groups(g2).maxSize
        s <- students1
      } yield Switch(s, g2)

      val refills = for {
        (g, _) <- assignment.groupMap filter { _._2.isEmpty }
        size  = groups(g).minSize
        queue = assignment.queue
        if size >= queue.size
        selection <- queue combinations size
      } yield FillGroup(g, selection.toSet)

      val drops = if (course.dropGroups) {
        for (g <- assignment.trueGroups.keys) yield DropGroup(g)
      } else Nil

      (swaps ++ switches ++ refills ++ drops).toArray
  }
}