package assigner.search

import assigner.model._

import org.coinor.opents._

/**
 * This is the core of the algorithm - an objective function for evaluating
 * different solutions based on various global and local criteria.
 */
class Objective(course: Course) extends ObjectiveFunction {
  val students = course.studentMap
  val groups   = course.groupMap
  val skills   = course.skills
  // default scale
  val friendsAndFoesScale   = 0.5
  val groupPreferencesScale = 1.0
  
  val criteria = Map(
    if (course.settings.diverse) "maximallyDiverse" -> maximallyDiverse _
    else                         "evenlySkilled"    -> evenlySkilled _,
    "groupPreferences" -> groupPreferences(useWeights = true) _,
    "friendsAndFoes"   -> friendsAndFoes  (useWeights = true) _)

  val withWeights = for {
    (key, fun) <- criteria
    weight = course.weights.getOrElse(key, 1.0)
    if weight != 0
  } yield key -> (weight, fun)

  def evaluate(solution: Solution, move: Move) =
    if (move == null) {
      Array(score(solution))
    } else {
      val assignment = solution.asInstanceOf[Assignment].clone
      move.operateOn(assignment)
      Array(score(assignment))
    }

  /**  @return the weighted overall score of `solution` */
  def score(solution: Solution) = components(solution).values.sum

  /**
   * Calculate all components of the score separately.
   * @param solution the assignment to test
   * @return a [[Map]] of criteria keys -> component score
   */
  def components(solution: Solution): Map[String, Double] = solution match {
    case assignment: Assignment =>
      withWeights mapValues { case (w, f) => w * f(assignment) }
    case _ => Map.empty
  }


  /**
   * Calculate the score of this assignment for even distribution of skills.
   * This is a global criterion, only one of which should be enabled at a time.
   * @param assignment the assignment to test
   * @return the score
   */
  def evenlySkilled(assignment: Assignment): Double = {
    val minSkills = {
      for {
        (g, ss)  <- assignment.trueGroups
        skillMaps = ss.toSeq.map { students(_).skills withDefaultValue 0.0 }
        skill    <- skills | groups(g).skills
      } yield skill -> skillMaps.map { _(skill) }.max
    } groupBy { _._1 } map { _._2.values.min }
    if (minSkills.isEmpty) 0 else minSkills.min * minSkills.sum
  }

  /**
   * Calculate the score of this assignment for maximal diversity of skills.
   * This is a global criterion, only one of which should be enabled at a time.
   * @param assignment the assignment to test
   * @return the score
   */
  def maximallyDiverse(assignment: Assignment): Double = {
    for {
      (g, ss)     <- assignment.trueGroups
      relevant     = skills | groups(g).skills
      Seq(s1, s2) <- ss.toSeq combinations 2
      skillMap1    = students(s1).skills withDefaultValue 0.0
      skillMap2    = students(s2).skills withDefaultValue 0.0
      skill       <- relevant
    } yield (skillMap1(skill) - skillMap2(skill)).abs
  }.sum

  /**
   * Calculate the score of this assignment for group preferences.
   * This is a local criterion, calculated per-student and summed up.
   * @param useWeights should local weights (per-student) be enabled?
   * @param assignment     the assignment to test
   * @return the score
   */
  def groupPreferences(useWeights: Boolean)
                      (assignment: Assignment): Double = {
    for {
      (s, g)   <- assignment.studentMap
      if !g.isQueue
      student   = students(s)
      weightMap = student.weights withDefaultValue 0.5
      weight    = if (useWeights) weightMap("preferences") else 1.0
      pref      = student.preferences.getOrElse(g, 0.0)
    } yield weight * pref * groupPreferencesScale
  }.sum

  /**
   * Calculate the score of this assignment for friends and foes.
   * This is a local criterion, calculated per-student and summed up.
   * @param useWeights should local weights (per-student) be enabled?
   * @param assignment the assignment to test
   * @return the score
   */
  def friendsAndFoes(useWeights: Boolean)
                    (assignment: Assignment): Double = {
    for {
      (s1, g)  <- assignment.studentMap
      if !g.isQueue
      student   = students(s1)
      weightMap = student.weights withDefaultValue 0.5
      weight    = if (useWeights) weightMap("friends") else 1.0
      s2       <- assignment studentsIn g
    } yield weight * {
      if      (student friends s2)  friendsAndFoesScale
      else if (student foes    s2) -friendsAndFoesScale
      else 0.0
    }
  }.sum
}
