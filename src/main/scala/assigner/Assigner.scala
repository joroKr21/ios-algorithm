package assigner

import org.coinor.opents.{BestEverAspirationCriteria, SimpleTabuList, SingleThreadedTabuSearch}

object Assigner extends App {

  val students = Set[Student](
    Student(0, "dss", true, Map("1" -> 5, "2" -> 3, "3" -> 4), List(2, 1, 0), Set(4), Set()),
    Student(1, "dss", true, Map("1" -> 1, "2" -> 5, "3" -> 2), List(1, 0, 2), Set(), Set(7)),
    Student(2, "dss", true, Map("1" -> 4, "2" -> 5, "3" -> 3), List(0, 2, 1), Set(), Set()),
    Student(3, "dss", true, Map("1" -> 2, "2" -> 4, "3" -> 5), List(0, 1, 2), Set(1), Set()),
    Student(4, "dss", true, Map("1" -> 4, "2" -> 4, "3" -> 3), List(2, 1, 0), Set(), Set()),
    Student(5, "dss", true, Map("1" -> 3, "2" -> 3, "3" -> 3), List(1, 2, 0), Set(3, 8), Set()),
    Student(6, "dss", true, Map("1" -> 1, "2" -> 4, "3" -> 3), List(0, 1, 2), Set(), Set()),
    Student(7, "dss", true, Map("1" -> 4, "2" -> 2, "3" -> 5), List(1, 2, 0), Set(), Set()),
    Student(8, "dss", true, Map("1" -> 1, "2" -> 2, "3" -> 1), List(2, 0, 1), Set(), Set(2)),
    Student(9, "dss", true, Map("1" -> 3, "2" -> 4, "3" -> 2), List(0, 2, 1), Set(), Set())
  ).map(s => s.id -> s).toMap

  val groups = Set[Group](
    Group(0, 3, 3, Set("1", "2", "3")),
    Group(1, 3, 3, Set("1", "2", "3")),
    Group(2, 4, 4, Set("1", "2", "3"))
  ).map(s => s.id -> s).toMap

  val tabuSearch = new SingleThreadedTabuSearch(
      new InitialMatching(students, groups),
      new AssignmentManager(students, groups),
      new Objective(students, groups),
      new SimpleTabuList(7),
      new BestEverAspirationCriteria,
      true
  )

  tabuSearch.setIterationsToGo(100)
  tabuSearch.startSolving()

  println(tabuSearch.getBestSolution)
  println(new Objective(students, groups).evaluate(tabuSearch.getBestSolution, null)(0))
}
