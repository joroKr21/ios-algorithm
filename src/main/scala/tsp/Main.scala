package tsp

import org.coinor.opents._

object Main extends App {

  val left =  Array[Double](72, 40, 31, 20, 66, 89, 75, 47, 38, 17, 31, 79, 139, 189, 166, 183, 162, 119, 20, 18)
  val right = Array[Double](186, 170, 153, 188, 138, 127, 99, 69, 50, 37, 13, 46, 9, 13, 65, 70, 95, 182, 118, 111)

  // Initialize our objects
//  val r: Random = new Random(12345)
//  val customers = Array.fill[Double](20, 2) {
//    r.nextDouble * 200
//  }

  val customers = Array.ofDim[Double](20,2)
  for(i <- 0 until customers.length) {
    customers(i)(0) = left(i)
    customers(i)(1) = right(i)
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
