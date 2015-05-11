
package object assigner {
  case class Student(id: Int, preferences: Set[Int], skills: Set[Int], friends: Set[Int], foes: Set[Int], mandatory: Boolean, group: Int = -1)

  abstract class Group {
    def skills: Set[Int]
    def size: Int
    var students: Set[Student] = Set()

    def addStudent(student: Student) : Boolean = {
      if(students.size == size) false
      else {
        students.+=(student)
        true
      }
    }
  }
  case class Project(skills: Set[Int], size: Int) extends Group
  case class WaitingList(skills: Set[Int], size: Int) extends Group
}