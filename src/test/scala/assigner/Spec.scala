package assigner

import assigner.model._
import assigner.search._

import org.junit.runner.RunWith
import org.scalacheck.Gen._
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[JUnitRunner])
class Spec extends FunSuite with Matchers with PropertyChecks with DataGen {

  val courses = courseGen(
      settings       = Settings(),
      endpoints      = Endpoints("http://localhost", "http://localhost"),
      numStudentsGen = choose(8, 12),
      numGroupsGen   = const(2),
      numSkillsGen   = choose(2, 3))

  test("Let's see") {
    forAll(courses) { course: Course =>
      whenever(course.validate forall { !_.isInstanceOf[Error] }) {
        val normalized = course
        val assigner = new Assigner(normalized)
        val bruteForce = new BruteForce(normalized)
        val objective = new Objective(normalized)
        val best1 = assigner.solution
        val best2 = bruteForce.solution
        if (objective.score(best1) > objective.score(best2)) {
          println(best1)
          println(best2)
          println(best1 == best2)
        }
      }
    }
  }
}
