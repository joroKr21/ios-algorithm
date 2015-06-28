package assigner

// TODO: Implement brute force solution
class BruteForce(course: Course) {

  val students = course.studentMap
  var groups = course.groupMap
  val assignment = Assignment(Map.empty, Map.empty)

  def assign(students: Map[Int, Student], groups: Map[Int, Group]) = for {
    g <- groups.values
    n <- g.minSize to g.maxSize
    ss <- students.values.toStream combinations n
  } yield ()
}
