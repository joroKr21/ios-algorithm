package assigner.model

import assigner._

/**
 * Store all relevant immutable data about a course.
 * @param id        job ID associated with this course, should be unique
 * @param settings  configuration data for the algorithm instance
 * @param endpoints endpoint URLs for posting back the result
 * @param students  [[List]] of all [[Student]]s in the course
 * @param groups    [[List]] of all [[Group]]s in the course
 * @param skills    [[Set]] of global skills relevant for all groups
 * @param weights   global weights for components of the objective function
 */
case class Course(
    id:        Long,
    settings:  Settings,
    endpoints: Endpoints,
    students:  List[Student],
    groups:    List[Group],
    skills:    Set[String]        = default.courseSkills,
    weights:   Map[String, Double] = default.globalWeights) {

  /** Should we consider dropping groups in the algorithm? */
  def dropGroups = students.size < groups.sumBy { _.minSize }

  /** @return map of student ID -> student */
  def studentMap: Map[Long, Student] =
    students.map { s => s.id -> s }.toMap

  /** @return map of group ID -> group */
  def groupMap: Map[Long, Group] =
    groups.map { g => g.id -> g }.toMap

  /** @return `true` if global weights are enabled for this course */
  def hasGlobalWeights: Boolean = weights.nonEmpty

  /**
   * Normalize all weights in the course.
   * @param scale all weights will lie in [-scale, scale]
   * @return this course with all weights normalized
   */
  def normalized(scale: Double = default.scale): Course = copy(
    students = students map { _ normalized scale },
    weights  = weights normalized scale)

  /**
   * Validate this course's data.
   * Errors will prevent the algorithm from running.
   * Warnings can be ignored, but are probably faulty input.
   * @return a sequence of any warnings and/or errors in the data.
   */
  def validate: Validation = {
    val set =  settings.validate
    val end = endpoints.qualify().validate
    val ss  = students.map { _.validate }.foldLeft(succ()) { _ merge _ }
    val gs  = groups  .map { _.validate }.foldLeft(succ()) { _ merge _ }

    val sameSs = students.map { _.id }.freq.collect {
      case (s, n) if n > 1 => warn(s"Student $id occurs $n times in the data")
    }.foldLeft(succ()) { _ merge _ }

    val sameGs = groups.map { _.id }.freq.collect {
      case (g, n) if n > 1 => warn(s"Group $id occurs $n times in the data")
    }.foldLeft(succ()) { _ merge _ }

    val mandatoryStudents = students count { _.mandatory }
    val mandatoryGroups   = groups  filter { _.mandatory } sumBy { _.minSize }
    val minGroups = groups sumBy { _.minSize }
    val maxGroups = groups sumBy { _.maxSize }

    val tooMany = maybeErr(mandatoryStudents > maxGroups,
      s"Not enough spots for all mandatory students: $mandatoryStudents")

    val tooFew = maybeErr(students.size < mandatoryGroups,
      s"Not enough students to fill all mandatory groups: $mandatoryGroups")

    val notEnough = maybeWarn(students.size < minGroups,
      s"Not enough students to fill all groups: $minGroups, some will be dropped")

    val studentSkills = students.flatMap { _.skills.keys }.toSet
    val globalSkills  = skills | groups.flatMap { _.skills }.toSet

    val unrelated = maybeWarn((studentSkills &~ globalSkills).nonEmpty,
      "Unrelated skills found in student data")

    Seq(set, end, ss, gs, sameSs, sameGs, tooMany, tooFew, notEnough, unrelated) reduce {
      _ merge _
    }
  }
}
