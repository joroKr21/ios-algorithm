package assigner

import org.coinor.opents.{Move, ObjectiveFunction, Solution}
import assigner._

case class Objective(course: Course) extends ObjectiveFunction {
  val students = course.students.map(s => s.id -> s).toMap
  val groups = course.groups.map(g => g.id -> g).toMap
  val skills = course.skills

  lazy val globalAvg = avgSkills(students.values)

  val globalCriteria = Map(
    "maximallyDiverse" -> maximallyDiverse _,
    "equallySkilled" -> equallySkilled _)

  val localCriteria = Map(
    "groupPreferences" -> groupPreferences _,
    "friendsAndFoes" -> friendsFoes _
  )

  override def evaluate(solution: Solution, move: Move): Array[Double] = {
    def eval(solution: Solution) = solution match {
      case sol: Assignment =>
        val G = equallySkilled(sol)
        val F = Fs(sol)
        val out = (G + F).toString

        logger.info(s"New Solution: $out")
        Array(G + F)
    }

    if (move == null) eval(solution)
    else {
      val newSol = solution.asInstanceOf[Assignment].copy
      move.operateOn(newSol)
      eval(newSol)
    }
  }

  def maximallyDiverse(solution: Assignment): Double = {
    val minSkills = solution.groupMap.flatMap {
      case (g, ss) => groups(g).skills.map { skill =>
        skill -> ss.map(s => students(s).skills(skill)).max
      }.toMap
    }.groupBy(_._1).mapValues(_.values.min)

    val Gs = minSkills.values.min * minSkills.values.sum

    Gs.toDouble
  }

  def Fs(solution: Assignment): Double = {
    friendsFoes(solution)
  }

  def friendsFoes(solution: Assignment): Double = {
    solution.groupMap.keys.map { key =>
      val score = for {
        studId1 :: studId2 :: _ <- solution.studentMap.filter(_._2 == key).keys.toList.combinations(2)
        student1 = students(studId1)
        student2 = students(studId2)
      } yield {
          val friends = student1.friends.contains(studId2) || student2.friends.contains(studId1)
          val foes = student1.foes.contains(studId2) || student2.foes.contains(studId1)
          if (friends && foes) {
            -.5
          } else if (friends) {
            if (student1.friends.contains(studId2) && student2.friends.contains(studId1)) 1
            else .5
          } else if (foes) {
            if (student1.foes.contains(studId2) && student2.foes.contains(studId1)) -2
            else -1
          } else {
            0
          }
        }

      score.sum
    }.sum
  }

  def groupPreferences(solution: Assignment) = {
    solution.studentMap.map {
      case (_, -1) => 0
      case (s,  g) => - students(s).groupPreferences.indexOf(g)
    }.sum
  }

  def equallySkilled(solution: Assignment): Double = {
    val averages = solution.groupMap.map {
      case (_, ss) => avgSkills(ss.map(students))
    }

    1 / averages.map { _ - globalAvg }.map { x => x * x }.mean
  }

  def avgSkills(students: Traversable[Student]) =
    students.map { s => skills.map(s.skills).sum.toDouble }.mean
}