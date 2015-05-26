package assigner

import org.coinor.opents.{Move, Solution, ObjectiveFunction}

case class Objective(students: Map[Int, Student], groups: Map[Int, Group]) extends ObjectiveFunction {
  override def evaluate(solution: Solution, move: Move): Array[Double] = {
    def eval(solution: Solution) = solution match {
      case sol: Assignment =>
        val minSkills = sol.groupMap.flatMap {
          case (g, ss) => groups(g).skills.map { skill =>
            skill -> ss.map(s => students(s).skills(skill)).max
          }.toMap
        }.groupBy(_._1).mapValues(_.values.min)

        val Gs = minSkills.values.min * minSkills.values.sum
        println(Gs)
        Array(Gs.toDouble)
    }

    if (move == null) eval(solution)
    else {
      val newSol = solution.asInstanceOf[Assignment].copy()
      move.operateOn(newSol)
      eval(newSol)
    }
  }
}