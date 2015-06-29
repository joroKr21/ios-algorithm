package assigner.model

import assigner._

/**
 * Store all relevant immutable data about a student.
 * @param id ID of this student, should be unique
 * @param name name of this student
 * @param mandatory may this student be excluded from the course?
 * @param skills weighted [[Map]] of this student's skills
 * @param weights importance over components of the objective function
 * @param preferences weighted preferences over some or all of the groups
 * @param friends [[Set]] of the student IDs of all friends
 * @param foes [[Set]] of the student IDs of all foes
 */
case class Student(
                    id: StudentId,
                    name: String = default.name,
                    mandatory: Boolean = default.mandatory,
                    skills: Map[SkillId, Double] = default.studentSkills,
                    weights: Map[String,  Double] = default.localWeights,
                    preferences: Map[GroupId, Double] = default.preferences,
                    friends: Set[StudentId] = default.friends,
                    foes: Set[StudentId] = default.foes) {
  
  /**
   * Validate this student's data.
   * [[Error]]s will prevent the algorithm from running.
   * [[Warning]]s can be ignored, but are probably faulty input.
   * @return a sequence of any [[Warning]]s and [[Error]]s in the data.
   */
  def validate: Seq[Validation] = {
    val emptySpff = maybeWarn(
      Seq(skills, preferences, friends, foes).forall(_.isEmpty),
      s"Student $id has no skills / preferences / friends / foes")

    val emptyWeights =
      maybeWarn(weights.isEmpty, s"Student $id has no weights")

    val negSkills = skills.collect { case (s, w) if w < 0 =>
      err(s"Skill $s of student $id has a negative weight of $w")
    }

    val negWeights = weights.collect { case (c, w) if w < 0 =>
      err(s"The $c of student $id have a negative weight of $w")
    }

    val negPrefs = preferences.collect { case (g, w) if w < 0 =>
      err(s"Preference $g of student $id has a negative weight of $w")
    }

    flatten(emptySpff, emptyWeights, negSkills, negWeights, negPrefs)
  }
}
