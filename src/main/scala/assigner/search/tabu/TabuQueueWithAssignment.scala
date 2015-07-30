package assigner.search.tabu

import assigner.model.Student
import assigner.search.moves.{Switch, Swap}
import org.coinor.opents.{Move, Solution}

class TabuQueueWithAssignment(size: Int, fixedStudents: List[Student]) extends TabuQueue(size) {
  override def isTabu(solution: Solution, move: Move): Boolean = {
    queue.contains(move) && (move match {
      case swap: Swap => fixedStudents.contains(swap.student1) || fixedStudents.contains(swap.student2)
      case switch: Switch => fixedStudents.contains(switch.student)
    })
  }
}
