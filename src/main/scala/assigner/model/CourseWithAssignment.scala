package assigner.model

// TODO: Wrap up this class with comments and validation.
case class CourseWithAssignment(
    course:     Course,
    groupMap:   Map[Long,  Set[Long]]) {

    def validate: Validation = course.validate
}
