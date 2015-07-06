package assigner.model

import assigner._

/**
 * Configuration data for running an instance of the algorithm.
 * @param iterations     number of iterations without improvement to stop
 * @param startingPoints number of different initial assignments to check
 * @param tabuSize       size of the tabu list / queue
 * @param diverse        should the algorithm optimize diversity or equality?
 */
case class Settings(
    iterations:     Int     = default.iterations,
    startingPoints: Int     = default.startingPoints,
    tabuSize:       Int     = default.tabuSize,
    diverse:        Boolean = default.diverse) {

  /**
   * Validate the configuration.
   * [[Error]]s will prevent the algorithm from running.
   * [[Warning]]s can be ignored, but are probably faulty input.
   * @return a sequence of any [[Warning]]s and [[Error]]s in the data.
   */
  def validate: Seq[Validation] = {
    val negIterations = maybeErr(iterations <= 0,
      s"Non-positive number of iterations: $iterations")

    val negStartingPoints = maybeErr(startingPoints <= 0,
      s"Non-positive number of starting points: $startingPoints")

    val negTabuSize = maybeWarn(tabuSize <= 0,
      s"Tabu list will not be used due to a non-positive size of $tabuSize")

    val tabuIterations = maybeWarn(tabuSize >= iterations,
      s"The size of tabu list: $tabuSize > number of iterations: $iterations")

    flatten(negIterations, negStartingPoints, negTabuSize, tabuIterations)
  }
}
