package assigner.model

import assigner._

/**
 * Store all relevant immutable data about a group.
 * @param id        ID of this group, should be unique and != -1
 * @param minSize   minimal number of participants
 * @param maxSize   maximal number of participants
 * @param mandatory may this group be dropped from the course?
 * @param name      name of this group
 * @param skills    [[Set]] of skills relevant for this group
 */
case class Group(
    id:        Long,
    minSize:   Int,
    maxSize:   Int,
    mandatory: Boolean      = default.mandatory,
    name:      String       = default.name,
    skills:    Set[String] = default.groupSkills) {

  /**
   * Validate this group's data.
   * Errors will prevent the algorithm from running.
   * Warnings can be ignored, but are probably faulty input.
   * @return a sequence of any warnings and errors in the data.
   */
  def validate: Validation = {
    val queue = maybeErr(id.isQueue,
      s"Group $name has ID of $id, but it is used as the queue ID")

    val negMin = maybeWarn(minSize <= 0,
      s"Group $id has non-positive min size of $minSize")

    val zeroMax = maybeWarn(maxSize == 0,
      s"Group $id has max size of 0 and will be ignored")

    val negMax = maybeErr(maxSize < 0,
      s"Group $id has negative max size of $maxSize")

    val minGtMax = maybeErr(minSize > maxSize,
      s"Group $id has min size ($minSize) > max size ($maxSize)")

    Seq(queue, negMin, zeroMax, negMax, minGtMax) reduce { _ merge _ }
  }
}
