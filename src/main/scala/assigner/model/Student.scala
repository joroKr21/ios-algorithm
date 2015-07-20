package assigner.model

import assigner._

/**
 * Store all relevant immutable data about a student.
 * @param id          ID of this student, should be unique
 * @param name        name of this student
 * @param mandatory   may this student be excluded from the course?
 * @param skills      weighted [[Map]] of this student's skills
 * @param weights     importance over components of the objective function
 * @param preferences weighted preferences over some or all of the groups
 * @param friends     [[Set]] of the student IDs of all friends
 * @param foes        [[Set]] of the student IDs of all foes
 */
case class Student(
    id:          Long,
    name:        String               = default.name,
    mandatory:   Boolean              = default.mandatory,
    skills:      Map[String, Double] = default.studentSkills,
    weights:     Map[String,  Double] = default.localWeights,
    preferences: Map[Long, Double] = default.preferences,
    friends:     Set[Long]       = default.friends,
    foes:        Set[Long]       = default.foes) {

  /**
   * Normalize all weights for this student.
   * @param scale all weights will lie in [-scale, scale]
   * @return this student with all weights normalized
   */
  def normalized(scale: Double = default.scale): Student = copy(
    skills      = skills      normalized scale,
    weights     = weights     normalized scale,
    preferences = preferences normalized scale)
  
  /**
   * Validate this student's data.
   * Errors will prevent the algorithm from running.
   * Warnings can be ignored, but are probably faulty input.
   * @return a sequence of any warnings and errors in the data.
   */
  def validate: Validation = {
    val emptySpff = maybeWarn(
      Seq(skills, preferences, friends, foes) forall { _.isEmpty },
      s"Student $id has no skills / preferences / friends / foes")

    val emptyWeights =
      maybeWarn(weights.isEmpty, s"Student $id has no weights")

    val negSkills = skills.collect { case (s, w) if w < 0 =>
      err(s"Skill $s of student $id has a negative weight of $w")
    }.foldLeft(succ()) { _ merge _ }

    val negWeights = weights.collect { case (c, w) if w < 0 =>
      err(s"The $c of student $id have a negative weight of $w")
    }.foldLeft(succ()) { _ merge _ }

    val negPrefs = preferences.collect { case (g, w) if w < 0 =>
      err(s"Preference $g of student $id has a negative weight of $w")
    }.foldLeft(succ()) { _ merge _ }

    Seq(emptySpff, emptyWeights, negSkills, negWeights, negPrefs) reduce { _ merge _ }
  }
}
