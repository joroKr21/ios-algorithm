package assigner.search

import assigner._
import assigner.model._
import assigner.search.tabu._

import org.coinor.opents._

/** Entry point for running the algorithm. */
class Assigner(course: Course) {
  val c = course.normalized
  val settings = c.settings
  val iterations = settings.iterations

  // TODO: Test the impact of using MultiThreadedTabuSearch
  private val tabuSearch = new SingleThreadedTabuSearch(
    new StartingPoint(c),
    new Manager(c),
    new Objective(c),
    new TabuQueue(settings.tabuSize),
    new BestEverAspirationCriteria,
    true)

  // Add a listener in order to get the same behaviour as in the paper where they terminate the algorithm
  // when the algorithm is not improving the objective function for a pre-set number of moves
  tabuSearch.addTabuSearchListener(new TabuSearchAdapter {
    override def newCurrentSolutionFound(e: TabuSearchEvent) = {
      val cur = e.getTabuSearch.getObjectiveFunction.evaluate(e.getTabuSearch.getCurrentSolution, null)(0).toString
      val best = e.getTabuSearch.getObjectiveFunction.evaluate(e.getTabuSearch.getBestSolution, null)(0).toString


      logger.info(s"Current Value: $cur, Best Value: $best")
    }

    override def newBestSolutionFound(e: TabuSearchEvent) = {
      e.getTabuSearch.setIterationsToGo(iterations)
    }
  })

  tabuSearch.setIterationsToGo(iterations)

  def startSolving(): Assignment = {
    tabuSearch.startSolving()
    tabuSearch.getBestSolution.asInstanceOf[Assignment]
  }
}
