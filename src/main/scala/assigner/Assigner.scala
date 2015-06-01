package assigner

import org.coinor.opents._

case class Assigner(students: Map[Int, Student], groups: Map[Int, Group]) {

  val tabuSearch = new SingleThreadedTabuSearch(
      new InitialMatching(students, groups),
      new AssignmentManager(students, groups),
      new Objective(students, groups),
      new SimpleTabuList(7),
      new BestEverAspirationCriteria,
      true
  )

  tabuSearch.addTabuSearchListener(new TabuSearchAdapter {
    override def newCurrentSolutionFound(e: TabuSearchEvent) = {
      val cur = e.getTabuSearch.getObjectiveFunction.evaluate(e.getTabuSearch.getCurrentSolution, null)(0).toString
      val best = e.getTabuSearch.getObjectiveFunction.evaluate(e.getTabuSearch.getBestSolution, null)(0).toString


      logger.info(s"Current Value: $cur, Best Value: $best")
    }

    override def newBestSolutionFound(e: TabuSearchEvent) = {
      e.getTabuSearch.setIterationsToGo(20)
    }
  })

  tabuSearch.setIterationsToGo(20)
  tabuSearch.startSolving()
}
