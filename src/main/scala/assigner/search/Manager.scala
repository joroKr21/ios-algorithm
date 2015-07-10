package assigner.search

import assigner.model._
import assigner.search.moves._

import org.coinor.opents._

/** Generator of all possible moves from a given assignment. */
class Manager(course: Course) extends MoveManager {
  private val groups   = course.groupMap
  private val students = course.studentMap

  def getAllMoves(solution: Solution) = solution match {
    case assignment: Assignment =>
      val swaps = for {
        Seq(g1, g2) <- assignment.groupMap.keys.toSeq combinations 2
        s1          <- assignment studentsIn g1
        s2          <- assignment studentsIn g2
        if !(g1.isQueue && students(s2).mandatory)
        if !(g2.isQueue && students(s1).mandatory)
      } yield Swap(s1, s2)

      val switches = for {
        (g1, ss1) <- assignment.groupMap.iterator
        (g2, ss2) <- assignment.groupMap.iterator
        if g1 != g2 && !g2.isQueue
        if g1.isQueue || ss1.size > groups(g1).minSize
        if ss2.size >= groups(g2).minSize
        if ss2.size <  groups(g2).maxSize
        s <- ss1
      } yield Switch(s, g2)

      val refills = for {
        (g, ss) <- assignment.groupMap.iterator
        if !g.isQueue && ss.isEmpty
        size  = groups(g).minSize
        if size > 0
        queue = assignment.queue
        if size   <= queue.size
        selection <- queue subsets size
      } yield FillGroup(g, selection)

      val drops = if (course.dropGroups) for {
          g <- assignment.trueGroups.keys.iterator
          if !groups(g).mandatory
          if assignment studentsIn g forall { !students(_).mandatory }
        } yield DropGroup(g)
      else Iterator.empty

      (swaps ++ switches ++ refills ++ drops).toArray
  }
}