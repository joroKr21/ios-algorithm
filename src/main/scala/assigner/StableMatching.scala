package assigner

class StableMatching(students: List[Student], groups: List[Group]) extends MatchingSolution(students, groups) {
  val totalSize = groups.map(_.size).sum
  val mandatoryStudents = students.filter(_.mandatory)
  val otherStudents = students.filter(!_.mandatory).shuffle
  val selectedStudents = mandatoryStudents ++ otherStudents.take(totalSize - mandatoryStudents.size)

  matching = {
    selectedStudents.shuffle.map { student =>
      var finished = false
      var group = 0
      var studentId = 0

      while(!finished) {
        val assigned = groups(group).addStudent(student)
        if(assigned) {
          finished = true
          studentId  = student.id
        } else {
          group += 1
        }
      }

      studentId -> group
    }.toMap
  }
}


