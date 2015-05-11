package assigner

import org.coinor.opents.{SimpleTabuList, TabuList}

object Assigner extends App {

  val students = List[Student](
    Student(0, Set(2, 1, 3, 0), Set(5, 3, 4), Set(4), Set(), mandatory = true),
    Student(1, Set(3, 0, 2, 1), Set(1, 5, 2), Set(), Set(7), mandatory = true),
    Student(2, Set(0, 1, 2, 3), Set(4, 5, 3), Set(), Set(), mandatory = true),
    Student(3, Set(1, 2, 3, 0), Set(2, 4, 5), Set(1), Set(), mandatory = true),
    Student(4, Set(3, 2, 1, 0), Set(4, 4, 3), Set(), Set(), mandatory = true),
    Student(5, Set(2, 3, 1, 0), Set(3, 3, 3), Set(3, 8), Set(), mandatory = true),
    Student(6, Set(0, 1, 3, 2), Set(1, 4, 3), Set(), Set(), mandatory = true),
    Student(7, Set(2, 0, 1, 3), Set(4, 2, 5), Set(), Set(), mandatory = true),
    Student(8, Set(3, 1, 2, 0), Set(1, 2, 1), Set(), Set(2), mandatory = true),
    Student(9, Set(0, 2, 3, 1), Set(3, 4, 2), Set(), Set(), mandatory = true)
  )

  val groups = List[Group](
    Project(Set(0, 1, 2), 3),
    Project(Set(0, 1, 2), 3),
    Project(Set(0, 1, 2), 4)
  )

  // TODO: Add objective function
  val stableMatching = new StableMatching(students, groups)
  // TODO: Add move-manager
  val tabuList: TabuList = new SimpleTabuList(7)

  //  Create Tabu Search object
  //  val tabuSearch: TabuSearch = new SingleThreadedTabuSearch(
  //    stableMatching,
  //    moveManager,
  //    objFunc,
  //    tabuList,
  //    new BestEverAspirationCriteria,
  //    false
  //  )

  //  // Start solving
  //  tabuSearch.setIterationsToGo(100)
  //  tabuSearch.startSolving()

  //  // Show solution
  //  val best: MySolution = tabuSearch.getBestSolution.asInstanceOf[MySolution]
  //  System.out.println("Best Solution:\n" + best)

  //  val tour: Array[Int] = best.tour
  //  for(i <- 0 until tour.length) {
  //    System.out.println(customers(tour(i))(0) + "\t" + customers(tour(i))(1))
  //  }
}
