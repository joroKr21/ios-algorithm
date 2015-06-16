package assigner

import org.coinor.opents._

case class Assigner(course: Course) {
  val settings = course.settings
  val iterations = settings.iterations
  val students = course.studentMap
  val groups = course.groupMap

  // TODO: Test the impact of using the MultiThreadedSearch
  private val tabuSearch = new SingleThreadedTabuSearch(
    new InitialMatching(students, groups),
    new AssignmentManager(course),
    new Objective(course),
    new SimpleTabuList(7),
    new BestEverAspirationCriteria,
    true
  )

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

  def startSolving() = {
    tabuSearch.startSolving()

    tabuSearch.getBestSolution.asInstanceOf[Assignment]
  }
}
