package assigner

/** This package contains classes for all domain-specific entities. */
package object model {

  /**
   * Contains the result from any data validation (may be recursive).
   * @param success  message if the data was valid (`""` by default)
   * @param warnings sequence of any warning messages (`Nil` by default)
   * @param errors   sequence of any error messages (`Nil` by default)
   */
  case class Validation(
      success:  String      = "",
      warnings: Seq[String] = Nil,
      errors:   Seq[String] = Nil) {

    /**
     * Merge all errors and warnings with the provided [[Validation]].
     * @param that the [[Validation]] to merge with
     * @return a new [[Validation]] containing all errors and/or warnings from both
     */
    def merge(that: Validation): Validation = Validation(
      if (that.success.isEmpty) success else that.success,
      that.warnings ++ warnings,
      that.errors   ++ errors)
  }

  /**
   * Factory method for successful [[Validation]]s.
   * @param message optional message to go with (`""` by default)
   * @return a success [[Validation]]
   */
  def succ(message: String = ""): Validation =
    Validation(success = message)

  /**
   * Factory method for [[Validation]]s containing a single warning.
   * @param message the warning message
   * @return a warning [[Validation]]
   */
  def warn(message: String): Validation =
    Validation(warnings = message :: Nil)

  /**
   * Factory method for [[Validation]]s containing a single error.
   * @param message the error message
   * @return an error [[Validation]]
   */
  def err (message: String): Validation =
    Validation(errors = message :: Nil)

  /**
   * Factory method for [[Validation]]s containing a single warning, based on a `condition`.
   * @param condition a predicate for testing the dangerous condition
   * @param message   the warning message
   * @return a warning if the `condition` was fulfilled, a success otherwise
   */
  def maybeWarn(condition: Boolean, message: String): Validation =
    if (condition) warn(message) else succ()

  /**
   * Factory method for [[Validation]]s containing a single error, based on a `condition`.
   * @param condition a predicate for testing the erroneous condition
   * @param message   the error message
   * @return an error if the `condition` was fulfilled, a success otherwise
   */
  def maybeErr(condition: Boolean, message: String): Validation =
    if (condition) err(message) else succ()

  /** Default arguments for domain-specific classes. */
  object default {
    val host           = "localhost"
    val port           = 8080
    val queueId        = -1l
    val name           = ""
    val mandatory      = false
    val diverse        = false
    val scale          = 10
    val iterations     = 20
    val initialMoves   = 100
    val startingPoints = 1
    val tabuSize       = 5
    val studentSkills  = Map.empty[String, Double]
    val groupSkills    = Set.empty[String]
    val courseSkills   = Set.empty[String]
    val localWeights   = Map.empty[String,  Double]
    val globalWeights  = Map.empty[String,  Double]
    val preferences    = Map.empty[Long, Double]
    val friends        = Set.empty[Long]
    val foes           = Set.empty[Long]
  }

  /** Syntactic sugar to check if a [[Long]] belongs to the waiting list. */
  implicit class IsQueue(val self: Long) extends AnyVal {
    /** @return true if this is the [[Long]] of the queue **/
    def isQueue = self == default.queueId
  }
}
