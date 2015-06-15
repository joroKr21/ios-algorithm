package assigner

import org.coinor.opents.{Move, ObjectiveFunction, Solution}

case class Objective(course: Course) extends ObjectiveFunction {
  val students = course.studentMap
  val groups   = course.groupMap
  val skills   = course.skills

  lazy val meanAvgSkills = avgSkills(students.values)

  val criteria = Map(if (course.settings.diverse)
    "maximallyDiverse" -> maximallyDiverse _ else
    "equallySkilled"   -> equallySkilled   _,
    "groupPreferences" -> groupPreferences(course.hasGlobalWeights) _,
    "friendsAndFoes"   -> friendsAndFoes  (course.hasGlobalWeights) _)

  val withWeights = criteria zipMap course.weights.filter { _._2 != 0 }

  override def evaluate(solution: Solution, move: Move) = {
    def eval(solution: Solution) = solution match {
      case assignment: Assignment =>
        val score = for {
          (name, (func, weight)) <- withWeights
        } yield weight * func(assignment)
        Array(score.sum)
    }

    if (move == null)
      eval(solution)
    else {
      val newSol = solution.asInstanceOf[Assignment].copy
      move.operateOn(newSol)
      eval(newSol)
    }
  }

  def maximallyDiverse(assignment: Assignment): Double = {
    val minSkills = assignment.groupMap flatMap {
      case (g, ss) => groups(g).skills.map { skill =>
        skill -> ss.map(students(_) skills skill).max
      }.toMap
    } groupBy { _._1 } map { _._2.values.min }
    minSkills.min * minSkills.sum
  }

  def equallySkilled(assignment: Assignment): Double = {
    def sqr(x: Double) = x * x
    val averages = assignment.groupMap map {
      case (_, ss) => avgSkills(ss map students)
    }

    1 / averages.map { x => sqr(x - meanAvgSkills) }.mean
  }

  def groupPreferences(localWeights: Boolean)
                      (assignment: Assignment): Double =
    assignment.studentMap.map {
      case (_, -1) => 0.0
      case (s,  g) =>
        val student = students(s)
        val weight  = if (localWeights)
          student.weights.getOrElse("preferences", 1.0) else 1.0

        - weight * math.log(student.preferences indexOf g)
    }.sum

  def friendsAndFoes(localWeights: Boolean)
                    (assignment: Assignment): Double =
    assignment.studentMap.map {
      case (_, -1) => 0.0
      case (s1, g) =>
        val student = students(s1)
        val weight  = if (localWeights)
          student.weights.getOrElse("friends", 1.0) else 1.0

        (for (s2 <- assignment.groupMap(g)) yield weight * {
          if      (student friends s2)  0.5
          else if (student foes    s2) -0.5
          else                          0.0
        }).sum
    }.sum

  def avgSkills(students: Traversable[Student]) =
    students.map { s => skills.map(s.skills).sum.toDouble }.mean
}