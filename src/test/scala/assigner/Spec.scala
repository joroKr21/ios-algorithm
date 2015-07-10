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
      numStudentsGen = choose(6, 9),
      numGroupsGen   = const(3),
      numSkillsGen   = choose(2, 3),
      groupSizeGen   = minMaxGroupSizeGen(2, 3))

  test("Let's see") {
    var difference = 0.0
    var courseCount = 0
    forAll(courses) { course: Course =>
      whenever(course.validate forall { !_.isInstanceOf[Error] }) {
        courseCount = courseCount + 1
        val normalized = course.normalized(1)
        val assigner = new Assigner(normalized)
        val bruteForce = new BruteForce(normalized)
        val objective = new Objective(normalized)
        val best1 = assigner.solution
        val best2 = bruteForce.solution
        val score1 = objective.score(best1)
        val score2 = objective.score(best2)
        difference = difference + (score1 / score2)
        if (score1 > score2) {
          println(best1)
          println(best2)
          if(best1 == best2) {
            val a = 0
          }
          println(score1)
          println(score2)
          println("----------------------")
        }
      }
    }

    println(difference / courseCount)
  }
}
