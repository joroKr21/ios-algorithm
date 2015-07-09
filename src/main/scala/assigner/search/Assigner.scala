package assigner.search

import assigner.model._
import assigner.search.tabu._

import org.coinor.opents._

/** Entry point for running the algorithm. */
class Assigner(course: Course) {
  val manager    = new Manager  (course)
  val objective  = new Objective(course)

  def solution: Assignment = {
    // TODO: Test the impact of using MultiThreadedTabuSearch
    val tabuSearch = new SingleThreadedTabuSearch(
      StartingPoint(course),
      manager, objective,
      new TabuQueue(course.settings.tabuSize),
      new BestEverAspirationCriteria,
      true)

    // Add a listener in order to get the same behaviour as in the paper where
    // they terminate the algorithm when the algorithm is not improving the
    // objective function for a pre-set number of moves
    tabuSearch.addTabuSearchListener(new TabuSearchAdapter {
//      override def newCurrentSolutionFound(e: TabuSearchEvent) = {
//        val search    = e.getTabuSearch
//        val objective = search.getObjectiveFunction
//        val current   = search.getCurrentSolution
//        val best      = search.getBestSolution
//        val curVal    = objective.evaluate(current, null)(0).toString
//        val bestVal   = objective.evaluate(best,    null)(0).toString
//        logger.info(s"Current value: $curVal, best value: $bestVal")
//      }

      override def newBestSolutionFound(e: TabuSearchEvent) = {
        e.getTabuSearch.setIterationsToGo(course.settings.iterations)
      }
    })

    tabuSearch.setIterationsToGo(course.settings.iterations)
    tabuSearch.startSolving()
    tabuSearch.getBestSolution.asInstanceOf[Assignment]
  }
}
