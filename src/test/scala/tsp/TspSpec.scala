package tsp

import org.coinor.opents._
import org.coinor.tsp._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TspSpec extends FlatSpec with Matchers {

  "Single threaded TSP in Java and Scala" should "yield similar results" in {
    val left  = Array[Double](72, 40, 31, 20, 66, 89, 75, 47, 38, 17, 31, 79, 139, 189, 166, 183, 162, 119, 20, 18)
    val right = Array[Double](186, 170, 153, 188, 138, 127, 99, 69, 50, 37, 13, 46, 9, 13, 65, 70, 95, 182, 118, 111)

    val tabuList  = new SimpleTabuList(7)
    val customers = Array.ofDim[Double](20, 2)
    for (i <- customers.indices) {
      customers(i)(0) = left(i)
      customers(i)(1) = right(i)
    }

    val sManager   = new Manager
    val sObjective = new Objective(customers)
    val sInitial   = new GreedyStart(customers)

    // create tabu search object
    val sTabuSearch = new SingleThreadedTabuSearch(
      sInitial, sManager, sObjective, tabuList,
      new BestEverAspirationCriteria, false)

    // start solving
    sTabuSearch.setIterationsToGo(100)
    sTabuSearch.startSolving()

    val jManager   = new MyMoveManager
    val jObjective = new MyObjectiveFunction(customers)
    val jInitial   = new MyGreedyStartSolution(customers)

    // create tabu search object
    val jTabuSearch = new SingleThreadedTabuSearch(
      jInitial, jManager, jObjective, tabuList,
      new BestEverAspirationCriteria, false)

    // start solving
    jTabuSearch.setIterationsToGo(100)
    jTabuSearch.startSolving()

    val sBest = sTabuSearch.getBestSolution.asInstanceOf[TravelPlan]
    val jBest = jTabuSearch.getBestSolution.asInstanceOf[MySolution]

    val sEval = sObjective.evaluate(sBest, null)(0)
    val jEval = jObjective.evaluate(jBest, null)(0)

    sEval / jEval should equal (1.0 +- 0.05)
    sBest.tour should have size customers.length
    jBest.tour should have size customers.length
    sBest.tour should contain theSameElementsAs jBest.tour
  }

  "Multi threaded TSP in Java and Scala" should "yield similar results" in {
    val left  = Array[Double](72, 40, 31, 20, 66, 89, 75, 47, 38, 17, 31, 79, 139, 189, 166, 183, 162, 119, 20, 18)
    val right = Array[Double](186, 170, 153, 188, 138, 127, 99, 69, 50, 37, 13, 46, 9, 13, 65, 70, 95, 182, 118, 111)

    val tabuList  = new SimpleTabuList(7)
    val customers = Array.ofDim[Double](20, 2)
    for (i <- customers.indices) {
      customers(i)(0) = left(i)
      customers(i)(1) = right(i)
    }

    val sManager   = new Manager
    val sObjective = new Objective(customers)
    val sInitial   = new GreedyStart(customers)

    // create tabu search object
    val sTabuSearch = new MultiThreadedTabuSearch(
      sInitial, sManager, sObjective, tabuList,
      new BestEverAspirationCriteria, false)

    // start solving
    sTabuSearch.setIterationsToGo(100)
    sTabuSearch.startSolving()

    val jManager   = new MyMoveManager
    val jObjective = new MyObjectiveFunction(customers)
    val jInitial   = new MyGreedyStartSolution(customers)

    // create tabu search object
    val jTabuSearch = new MultiThreadedTabuSearch(
      jInitial, jManager, jObjective, tabuList,
      new BestEverAspirationCriteria, false)

    // start solving
    jTabuSearch.setIterationsToGo(100)
    jTabuSearch.startSolving()

    val sBest = sTabuSearch.getBestSolution.asInstanceOf[TravelPlan]
    val jBest = jTabuSearch.getBestSolution.asInstanceOf[MySolution]

    val sEval = sObjective.evaluate(sBest, null)(0)
    val jEval = jObjective.evaluate(jBest, null)(0)

    sEval / jEval should equal (1.0 +- 0.05)
    sBest.tour should have size customers.length
    jBest.tour should have size customers.length
    sBest.tour should contain theSameElementsAs jBest.tour
  }
}
