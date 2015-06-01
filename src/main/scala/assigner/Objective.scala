package assigner

import org.coinor.opents.{Move, ObjectiveFunction, Solution}

case class Objective(students: Map[Int, Student], groups: Map[Int, Group]) extends ObjectiveFunction {
  override def evaluate(solution: Solution, move: Move): Array[Double] = {
    def eval(solution: Solution) = solution match {
      case sol: Assignment =>
        val G = Gs(sol)
        val F = Fs(sol)

        println(G+F)
        Array(G+F)
    }

    if (move == null) eval(solution)
    else {
      val newSol = solution.asInstanceOf[Assignment].copy()
      move.operateOn(newSol)
      eval(newSol)
    }
  }

  def Gs(solution: Assignment) : Double = {
    val minSkills = solution.groupMap.flatMap {
      case (g, ss) => groups(g).skills.map { skill =>
        skill -> ss.map(s => students(s).skills(skill)).max
      }.toMap
    }.groupBy(_._1).mapValues(_.values.min)

    val Gs = minSkills.values.min * minSkills.values.sum

    Gs.toDouble
  }

  def Fs(solution: Assignment) : Double = {
    friendsFoes(solution)
  }

  def friendsFoes(solution: Assignment) : Double = {
    solution.groupMap.keys.map { key =>
      val score = for {
        studId1 :: studId2 :: _ <- solution.studentMap.filter(_._2 == key).keys.toList.combinations(2)
        student1 = students(studId1)
        student2 = students(studId2)
      } yield {
        val friends = student1.friends.contains(studId2) || student2.friends.contains(studId1)
        val foes = student1.foes.contains(studId2) || student2.foes.contains(studId1)
        if(friends && foes) {
          -.5
        } else if(friends) {
          if(student1.friends.contains(studId2) && student2.friends.contains(studId1)) 1
          else .5
        } else if(foes) {
          if(student1.foes.contains(studId2) && student2.foes.contains(studId1)) -2
          else -1
        } else {
          0
        }
      }

      score.sum
    }.sum
  }

}