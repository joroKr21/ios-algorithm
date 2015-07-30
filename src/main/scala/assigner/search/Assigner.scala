package assigner.search

import assigner.model._
import assigner.search.tabu._

import org.coinor.opents._

/** Entry point for running the algorithm. */
class Assigner(course: Course) {
  val manager    = new Manager  (course)
  val objective  = new Objective(course)

  def solution: (Assignment, Seq[(Move, Double)], Assignment) = {
    var log        = List.empty[(Move, Double)]
    val initial    = StartingPoint(course)
    // TODO: Test the impact of using MultiThreadedTabuSearch
    val tabuSearch = new SingleThreadedTabuSearch(
      initial.clone, manager, objective,
      new TabuQueue(course.settings.tabuSize),
      new BestEverAspirationCriteria,
      true)

    // Add a listener in order to get the same behaviour as in the paper where they terminate the
    // algorithm when the algorithm is not improving the objective function for a pre-set number of
    // moves
    tabuSearch.addTabuSearchListener(new TabuSearchAdapter {
      override def newCurrentSolutionFound(e: TabuSearchEvent) = {
        e.getTabuSearch.getCurrentSolution match {
          case assignment: Assignment =>
            log = (assignment.lastMove -> assignment.getObjectiveValue.head) :: log
          case _ =>
        }
      }

      override def newBestSolutionFound(e: TabuSearchEvent) = {
        e.getTabuSearch.setIterationsToGo(course.settings.iterations)
      }
    })

    tabuSearch.setIterationsToGo(course.settings.iterations)
    tabuSearch.startSolving()
    val solution = tabuSearch.getBestSolution.asInstanceOf[Assignment]
    (initial, log.reverse, solution)
  }

  def swap(assignment: Assignment): (Assignment, Seq[(Move, Double)], Assignment) = {
    var log        = List.empty[(Move, Double)]
    val studInCourse = assignment.studentMap
      .filter { case (stud, group) => group == -1 }
      .keys
      .map(id => course.studentMap(id))
      .toList

    // TODO: Test the impact of using MultiThreadedTabuSearch
    val tabuSearch = new SingleThreadedTabuSearch(
      assignment.clone, manager, objective,
      new TabuQueueWithAssignment(course.settings.tabuSize, studInCourse),
      new BestEverAspirationCriteria,
      true)

    // Add a listener in order to get the same behaviour as in the paper where they terminate the
    // algorithm when the algorithm is not improving the objective function for a pre-set number of
    // moves
    tabuSearch.addTabuSearchListener(new TabuSearchAdapter {
      override def newCurrentSolutionFound(e: TabuSearchEvent) = {
        e.getTabuSearch.getCurrentSolution match {
          case assignment: Assignment =>
            log = (assignment.lastMove -> assignment.getObjectiveValue.head) :: log
          case _ =>
        }
      }

      override def newBestSolutionFound(e: TabuSearchEvent) = {
        e.getTabuSearch.setIterationsToGo(course.settings.iterations)
      }
    })

    tabuSearch.setIterationsToGo(course.settings.iterations)
    tabuSearch.startSolving()
    val solution = tabuSearch.getBestSolution.asInstanceOf[Assignment]
    (assignment, log.reverse, solution)
  }

//  def swap(assignment: Assignment) = {
//    var log = List.empty[(Move, Double)]
//
//    val emptySpots = (for {
//      (groupId, students) <- assignment.groupMap
//      spots <- 0 until course.groupMap(groupId).maxSize - students.size
//    } yield groupId).toList
//    val waiting = assignment.waitingList
//
//    waiting
//      .combinations(emptySpots.size)
//      .toList
//      .permutations
//      .map(_.map { per =>
//        val assgn = emptySpots.zip(per) ++ assignment.studentMap
//
//    })
//  }
}
