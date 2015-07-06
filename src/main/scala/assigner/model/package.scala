package assigner

/** This package contains classes for all domain-specific entities. */
package object model {

  // ID types
  type CourseId  = Long
  type StudentId = Long
  type GroupId   = Long
  type SkillId   = String

  // for validation purposes
  sealed trait Validation { def message: String }
  case class Success(message: String) extends Validation
  case class Warning(message: String) extends Validation
  case class Error  (message: String) extends Validation

  // shorthands for validation
  def succ(message: String): Success = Success(message)
  def warn(message: String): Warning = Warning(message)
  def err (message: String): Error   = Error  (message)

  def maybeWarn(condition: Boolean, message: String): Seq[Warning] =
    if (condition) warn(message).sng else Nil

  def maybeErr(condition: Boolean, message: String): Seq[Error] =
    if (condition) err(message).sng else Nil

  /** Default arguments for domain-specific classes. */
  object default {
    val queueId        = -1l
    val name           = ""
    val mandatory      = false
    val diverse        = false
    val iterations     = 20
    val startingPoints = 1
    val tabuSize       = 5
    val studentSkills  = Map.empty[SkillId, Double]
    val groupSkills    = Set.empty[SkillId]
    val courseSkills   = Set.empty[SkillId]
    val localWeights   = Map.empty[String, Double]
    val globalWeights  = Map.empty[String, Double]
    val preferences    = Map.empty[GroupId, Double]
    val friends        = Set.empty[StudentId]
    val foes           = Set.empty[StudentId]
  }

  implicit class IsQueue(val self: GroupId) extends AnyVal {
    /** @return true if this is the [[GroupId]] of the queue **/
    def isQueue = self == default.queueId
  }
}
