package tsp

import org.coinor.opents._

object Tsp extends App {

  val left  = Array[Double](72, 40, 31, 20, 66, 89, 75, 47, 38, 17, 31, 79, 139, 189, 166, 183, 162, 119, 20, 18)
  val right = Array[Double](186, 170, 153, 188, 138, 127, 99, 69, 50, 37, 13, 46, 9, 13, 65, 70, 95, 182, 118, 111)

  val customers = Array.ofDim[Double](20, 2)
  for(i <- customers.indices) {
    customers(i)(0) = left(i)
    customers(i)(1) = right(i)
  }

  val objective = new Objective(customers)
  val initial   = new GreedyStart(customers)
  val manager   = new Manager
  val tabuList  = new SimpleTabuList(7)

  // create tabu search object
  val tabuSearch = new SingleThreadedTabuSearch(
    initial, manager, objective, tabuList,
    new BestEverAspirationCriteria, false)

  // start solving
  tabuSearch.setIterationsToGo(100)
  tabuSearch.startSolving()

  // Show solution
  val best = tabuSearch.getBestSolution.asInstanceOf[TravelPlan]
  println(s"Best Solution:\n$best")

  val tour = best.tour
  for(i <- tour.indices)
    println(s"${customers(tour(i))(0)}\t${customers(tour(i))(1)}")
}
