package assigner.model

// TODO: Wrap up this class with comments and validation.
case class CourseWithAssignment(
    course:     Course,
    studentMap: Map[Long, Long]       = Map.empty,
    groupMap:   Map[Long,  Set[Long]] = Map.empty) {
}
