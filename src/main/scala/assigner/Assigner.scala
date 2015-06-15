package assigner

import org.coinor.opents._

case class Assigner(course: Course) {
  val settings = course.settings
  val iterations = settings.iterations
  val students = course.students.map(s => s.id -> s).toMap
  val groups = course.groups.map(g => g.id -> g).toMap

  val tabuSearch = new SingleThreadedTabuSearch(
      new InitialMatching(students, groups),
      new AssignmentManager(students, groups),
      new Objective(course),
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
      e.getTabuSearch.setIterationsToGo(iterations)
    }
  })

  tabuSearch.setIterationsToGo(iterations)
  tabuSearch.startSolving()
}
