package assigner

class InitialMatching(students: Map[Int, Student], groups: Map[Int, Group]) extends Assignment(null, null) {
  private val totalSize = groups.values.map(_.maxSize).sum
  private val mandatoryStudentIds = students.values.filter(_.mandatory).map(_.id)
  private val otherStudentIds = students.values.filter(!_.mandatory).map(_.id).toList.shuffle
  private val numOfOtherStudents = totalSize - mandatoryStudentIds.size
  private val orderedStudentIds =
    (mandatoryStudentIds ++ otherStudentIds.take(numOfOtherStudents)).toList.shuffle ++
      otherStudentIds.drop(numOfOtherStudents)

  private val currentGroupSizes = Array.fill(groups.size)(0)
  studentMap = orderedStudentIds.map { studentId =>
    val student = students(studentId)
    var finished = false
    var groupId = -1

    for {
      preference <- student.groupPreferences
      if currentGroupSizes(preference) < groups(preference).maxSize && !finished
    } {
      currentGroupSizes(preference) += 1
      finished = true
      groupId = preference
    }

    studentId -> groupId
  }.toMap

  groupMap = studentMap.groupBy(_._2).filter { _._1 != -1 }.map { case (key, value) => key -> value.keySet}
}


