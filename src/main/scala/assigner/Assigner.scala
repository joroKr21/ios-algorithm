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
      println(e.getTabuSearch.getCurrentSolution)
    }

    override def newBestSolutionFound(e: TabuSearchEvent) = {
      e.getTabuSearch.setIterationsToGo(20)
    }
  })

  tabuSearch.setIterationsToGo(20)
  tabuSearch.startSolving()
}
