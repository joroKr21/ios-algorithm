package assigner.model

import assigner._

/**
 * Store all relevant immutable data about a course.
 * @param jobId     job ID associated with this course, should be unique
 * @param settings  configuration data for the algorithm instance
 * @param endpoints endpoint URLs for posting back the result
 * @param students  [[List]] of all [[Student]]s in the course
 * @param groups    [[List]] of all [[Group]]s in the course
 * @param skills    [[Set]] of global skills relevant for all groups
 * @param weights   global weights for components of the objective function
 */
case class Course(
    jobId:     CourseId,
    settings:  Settings,
    endpoints: Endpoints,
    students:  List[Student],
    groups:    List[Group],
    skills:    Set[SkillId]        = default.courseSkills,
    weights:   Map[String, Double] = default.globalWeights) {

  /** Should we consider dropping groups in the algorithm? */
  lazy val dropGroups = students.size < groups.sumBy { _.minSize }

  /** @return map of student ID -> student */
  def studentMap: Map[StudentId, Student] =
    students.map { s => s.id -> s }.toMap

  /** @return map of group ID -> group */
  def groupMap: Map[GroupId, Group] =
    groups.map { g => g.id -> g }.toMap

  /** @return `true` if global weights are enabled for this course */
  def hasGlobalWeights: Boolean = weights.nonEmpty

  /** @return this course with all weights normalized */
  def normalized: Course = copy(
    students = students map { _.normalized },
    weights  = weights.normalized)

  /**
   * Validate this course's data.
   * [[Error]]s will prevent the algorithm from running.
   * [[Warning]]s can be ignored, but are probably faulty input.
   * @return a sequence of any [[Warning]]s and [[Error]]s in the data.
   */
  def validate: Seq[Validation] = {
    val set =  settings.validate
    val end = endpoints.validate
    val ss  = students flatMap { _.validate }
    val gs  = groups   flatMap { _.validate }

    val sameSs = students.map { _.id }.freq collect {
      case (s, n) if n > 1 =>
        warn(s"Student $jobId occurs $n times in the data")
    }

    val sameGs = groups.map { _.id }.freq collect {
      case (g, n) if n > 1 =>
        warn(s"Group $jobId occurs $n times in the data")
    }

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

    val unrelated = maybeWarn(
      (studentSkills &~ globalSkills).nonEmpty,
      "Unrelated skills found in student data")

    flatten(set, end, ss, gs, sameSs, sameGs, tooMany, tooFew, notEnough, unrelated)
  }
}
