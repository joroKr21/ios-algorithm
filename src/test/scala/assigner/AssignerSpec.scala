package assigner

import assigner.model._
import assigner.search._

import org.junit.runner.RunWith
import org.scalacheck.Gen._
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[JUnitRunner])
class AssignerSpec extends FlatSpec with Matchers with PropertyChecks with DataGen {

  val depth   = 3
  val courses = courseGen(
    settings       = Settings(),
    endpoints      = Endpoints("http://localhost", "http://localhost"),
    numStudentsGen = choose(20, 30),
    numGroupsGen   = const(5),
    numSkillsGen   = choose(2, 3),
    groupSizeGen   = minMaxGroupSizeGen(4, 6))

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 10, workers = 2)

  "tabu search" should "terminate with a correct number of results" in {
    val students = Set(
      Student(id = 0, name = "Student 0", mandatory = true, skills = Map("1" -> 5, "2" -> 3, "3" -> 4),
        preferences = Map(2l -> 3, 1l -> 2, 0l -> 1), friends = Set(4)),
      Student(id = 1, name = "Student 1", mandatory = true, skills = Map("1" -> 1, "2" -> 5, "3" -> 2),
        preferences = Map(1l -> 3, 0l -> 2, 2l -> 1), friends = Set()),
      Student(id = 2, name = "Student 2", mandatory = true, skills = Map("1" -> 4, "2" -> 5, "3" -> 3),
        preferences = Map(0l -> 3, 2l -> 2, 1l -> 1), friends = Set()),
      Student(id = 3, name = "Student 3", mandatory = true, skills = Map("1" -> 2, "2" -> 4, "3" -> 5),
        preferences = Map(0l -> 3, 1l -> 2, 2l -> 1), friends = Set(1, 2, 4, 5, 6, 7, 8, 9)),
      Student(id = 4, name = "Student 4", mandatory = true, skills = Map("1" -> 4, "2" -> 4, "3" -> 3),
        preferences = Map(2l -> 3, 1l -> 2, 0l -> 1), friends = Set()),
      Student(id = 5, name = "Student 5", mandatory = true, skills = Map("1" -> 3, "2" -> 3, "3" -> 3),
        preferences = Map(1l -> 3, 2l -> 2, 0l -> 1), friends = Set(3, 8)),
      Student(id = 6, name = "Student 6", mandatory = true, skills = Map("1" -> 1, "2" -> 4, "3" -> 3),
        preferences = Map(0l -> 3, 1l -> 2, 2l -> 1), friends = Set()),
      Student(id = 7, name = "Student 7", mandatory = true, skills = Map("1" -> 4, "2" -> 2, "3" -> 5),
        preferences = Map(1l -> 3, 2l -> 2, 0l -> 1), friends = Set()),
      Student(id = 8, name = "Student 8", mandatory = true, skills = Map("1" -> 1, "2" -> 2, "3" -> 1),
        preferences = Map(2l -> 3, 0l -> 2, 1l -> 1), friends = Set()),
      Student(id = 9, name = "Student 9", mandatory = true, skills = Map("1" -> 3, "2" -> 4, "3" -> 2),
        preferences = Map(0l -> 3, 2l -> 2, 1l -> 1), friends = Set())
    ).map { s => s.id -> s }.toMap

    val groups = Set(
      Group(id = 0, minSize = 0, maxSize = 3, name = "Group 0", skills = Set("1", "2", "3")),
      Group(id = 1, minSize = 3, maxSize = 3, name = "Group 1", skills = Set("1", "2", "3")),
      Group(id = 2, minSize = 3, maxSize = 3, name = "Group 2", skills = Set("1", "2", "3"))
    ).map { g => g.id -> g }.toMap

    val settings = Settings(diverse = false, iterations = 20)
    val course   = Course(1, settings, Endpoints("http://localhost", "http://localhost"),
      students.values.toList, groups.values.toList, Set("1", "2", "3"))

    val assigner   = new Assigner(course)
    val assignment = assigner.solution

    assignment.studentMap should have size students.size
    assignment.groupMap   should have size groups.size + 1
  }

  it should "find a local optimum of the objective function" in {
    forAll(courses) { course: Course =>
      whenever(course.validate forall { !_.isInstanceOf[Error] }) {
        val assigner     = new Assigner(course normalized 10)
        val manager      = assigner.manager
        val objective    = assigner.objective
        val solution     = assigner.solution
        val score        = objective score solution
        val alternatives = Stream.iterate(Iterator(solution), depth) {
          _ flatMap { assignment =>
            for (mv <- manager getAllMoves assignment) yield {
              val clone = assignment.clone
              mv.operateOn(clone)
              clone
            }
          }
        }

        all(alternatives.flatten map objective.score) should be <= score
      }
    }
  }
}
