package assigner.search

import assigner.model._

import org.coinor.opents._

/**
 * This is the core of the algorithm - an objective function for evaluating
 * different solutions based on various global and local criteria.
 */
case class Objective(course: Course) extends ObjectiveFunction {
  val students = course.studentMap
  val groups = course.groupMap
  val skills = course.skills
  // default scale
  val friendsAndFoesScale = 0.5
  val groupPreferencesScale = 1.0
  
  val criteria = Map(
    if (course.settings.diverse) "maximallyDiverse" -> maximallyDiverse _
    else "evenlySkilled" -> evenlySkilled _,
    "groupPreferences" -> groupPreferences(useWeights = true) _,
    "friendsAndFoes" -> friendsAndFoes(useWeights = true) _)

  val withWeights = for {
    (key, fun) <- criteria
    weight = course.weights.getOrElse(key, 1.0)
    if weight != 0
  } yield key -> (weight, fun)

  def evaluate(solution: Solution, move: Move) = {
    def eval(solution: Solution) =
      Array(components(solution).values.sum)

    if (move == null) {
      eval(solution)
    } else {
      val assign = solution.asInstanceOf[Assignment].clone
      move.operateOn(assign)
      eval(assign)
    }
  }

  /**
   * Calculate all components of the score separately.
   * @param solution the assignment to test
   * @return a [[Map]] of criteria keys -> component score
   */
  def components(solution: Solution): Map[String, Double] = solution match {
    case assign: Assignment =>
      withWeights.mapValues { case (w, f) => w * f(assign) }
    case _ => Map.empty
  }


  /**
   * Calculate the score of this assignment for even distribution of skills.
   * This is a global criterion, only one of which should be enabled at a time.
   * @param assign the assignment to test
   * @return the score
   */
  def evenlySkilled(assign: Assignment): Double = {
    val minSkills = {
      for {
        (g, ss) <- assign.trueGroups
        skillMaps = ss.toSeq.map(students(_).skills.withDefaultValue(0.0))
        skill <- skills | groups(g).skills
      } yield skill -> skillMaps.map(_(skill)).max
    }.groupBy(_._1).map(_._2.values.min)
    minSkills.min * minSkills.sum
  }

  /**
   * Calculate the score of this assignment for maximal diversity of skills.
   * This is a global criterion, only one of which should be enabled at a time.
   * @param assign the assignment to test
   * @return the score
   */
  def maximallyDiverse(assign: Assignment): Double = {
    for {
      (g, ss) <- assign.trueGroups.toSeq
      relevant = skills | groups(g).skills
      s1 #:: s2 #:: _ <- ss.toStream.combinations(2)
      skillMap1 = students(s1).skills.withDefaultValue(0.0)
      skillMap2 = students(s2).skills.withDefaultValue(0.0)
      skill <- relevant
    } yield (skillMap1(skill) - skillMap2(skill)).abs
  }.sum

  /**
   * Calculate the score of this assignment for group preferences.
   * This is a local criterion, calculated per-student and summed up.
   * @param useWeights should local weights (per-student) be enabled?
   * @param assign the assignment to test
   * @return the score
   */
  def groupPreferences(useWeights: Boolean)(assign: Assignment): Double = {
    for {
      (s, g) <- assign.studentMap
      if !g.isQueue
      student = students(s)
      weightMap = student.weights.withDefaultValue(0.5)
      weight = if (useWeights) weightMap("preferences") else 1.0
      pref = student.preferences.getOrElse(g, 0.0)
    } yield weight * pref * groupPreferencesScale
  }.sum

  /**
   * Calculate the score of this assignment for friends and foes.
   * This is a local criterion, calculated per-student and summed up.
   * @param useWeights should local weights (per-student) be enabled?
   * @param assign the assignment to test
   * @return the score
   */
  def friendsAndFoes(useWeights: Boolean)(assign: Assignment): Double = {
    for {
      (s1, g) <- assign.studentMap
      if !g.isQueue
      student = students(s1)
      weightMap = student.weights.withDefaultValue(0.5)
      weight = if (useWeights) weightMap("friends") else 1.0
      s2 <- assign.studentsIn(g)
    } yield weight * {
      if (student.friends(s2)) friendsAndFoesScale
      else if (student.foes(s2)) -friendsAndFoesScale
      else 0.0
    }
  }.sum
}
