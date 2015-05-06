package tsp

import org.coinor.opents._
import scala.util.Random

object Main extends App {

  // Initialize our objects
  val r: Random = new Random(12345)
  val customers = Array.fill[Double](20, 2) {
    r.nextDouble * 200
  }
  val objFunc: ObjectiveFunction = new MyObjectiveFunction(customers)
  val initialSolution: Solution = new MyGreedyStartSolution(customers)
  val moveManager: MoveManager = new MyMoveManager
  val tabuList: TabuList = new SimpleTabuList(7)

  // Create Tabu Search object
  val tabuSearch: TabuSearch = new SingleThreadedTabuSearch(
    initialSolution,
    moveManager,
    objFunc,
    tabuList,
    new BestEverAspirationCriteria,
    false
  )

  // Start solving
  tabuSearch.setIterationsToGo(100)
  tabuSearch.startSolving()

  // Show solution
  val best: MySolution = tabuSearch.getBestSolution.asInstanceOf[MySolution]
  System.out.println("Best Solution:\n" + best)

  val tour: Array[Int] = best.tour
  for(i <- 0 until tour.length) {
    System.out.println(customers(tour(i))(0) + "\t" + customers(tour(i))(1))
  }
}
