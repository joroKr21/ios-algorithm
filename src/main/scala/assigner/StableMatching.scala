package assigner

import scala.util.Random

class StableMatching(students: List[Student], groups: List[Group]) extends MatchingSolution(students, groups) {
  val totalSize = groups.map(_.size).sum
  val mandatoryStudents = students.filter(_.mandatory == true)
  val otherStudents = Random.shuffle(students.filter(_.mandatory == false))
  val selectedStudents = mandatoryStudents ++ otherStudents.take(totalSize - mandatoryStudents.size)

  matching = {
    Random.shuffle(selectedStudents).map { student =>
      var finished = false
      var group = 0
      var id = 0
      while(!finished) {
        val assigned = groups(group).addStudent(student)
        if(assigned) {
          finished = true
          id  = student.id
        } else {
          group += 1
        }
      }

      id -> group
    }.toMap
  }
}


