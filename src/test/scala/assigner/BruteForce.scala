package assigner


// TODO: Implement brute force solution
class BruteForce(course: Course) {

  val students = course.studentMap
  //var curStudents = course.studentMap
  var groups = course.groupMap
  var assignment = Map[Int, Set[Int]]()
  val curAssignment = scala.collection.mutable.Map[Int, Set[Int]]()
  val objective = Objective(course)
  var bestScore = 0.0
  var bestAssignment = assignment

  def checkScore() = {
    val asign = Map[Int, Set[Int]](curAssignment.toSeq: _*)
    val studentMap = asign.flatMap { case (id, ss) =>
      ss.map(s => s -> id).toMap
    }

    val score = objective.evalBruteForce(Assignment(studentMap, asign))
    if(score > bestScore) {
      bestScore = score
      bestAssignment = asign
    }
  }

  def combinations(students: Set[Int], number: Int) = {
    students.toList.combinations(number).toStream
  }

  def assign(students: Set[Int], group: Group) = for {
    n <- (group.minSize to group.maxSize).toIterator
    ss <- combinations(students, n)
    if n <= students.size
  } yield ss.toSet

  def startSolving() = {
    var finished = false
    var level = 0
    var streams = scala.collection.mutable.Map[Int, Iterator[Set[Int]]](0 -> assign(students.keys.toSet, groups.get(0).get))

    while (!finished) {
      if (!streams(0).hasNext) finished = true
      else if (!streams(level).hasNext) level = level - 1
      else {
        val s = streams(level).next()
        curAssignment(level) = s

        // Check the score of the assignment
        if ((level + 1) == groups.size) {
          checkScore()
        }

        // Generate the new stream for the next level
        else {
          level = level + 1
          val studentsLeft = students.keySet -- curAssignment.values.flatMap(x => x).toSet
          streams(level) = assign(studentsLeft, groups(level))
        }
      }
    }

    (bestScore, bestAssignment)
  }


  /*

  A: 3
  B: 3
  C: 3

  Students: 9

  1-3 -> A

   */
}
