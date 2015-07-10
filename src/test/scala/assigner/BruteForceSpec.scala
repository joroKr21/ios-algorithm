package assigner

import assigner.model._
import assigner.search._

import org.junit.runner.RunWith
import org.scalacheck.Gen._
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[JUnitRunner])
class BruteForceSpec extends FlatSpec with Matchers with PropertyChecks with DataGen {

  val courses = courseGen(
      settings       = Settings(),
      endpoints      = Endpoints("http://localhost", "http://localhost"),
      numStudentsGen = choose(6, 9),
      numGroupsGen   = const(3),
      numSkillsGen   = choose(2, 3),
      groupSizeGen   = minMaxGroupSizeGen(2, 3))

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 10, workers = 2)

  "brute force" should "generate better solutions than tabu search" in {
    forAll(courses) { course: Course =>
      whenever(course.validate forall { !_.isInstanceOf[Error] }) {
        val normalized = course normalized 10
        val assigner   = new Assigner  (normalized)
        val bruteForce = new BruteForce(normalized)
        val bestTS     =   assigner.solution
        val bestBF     = bruteForce.solution
        val scoreTS    =   assigner.objective score bestTS
        val scoreBF    = bruteForce.objective score bestBF
        scoreTS should be <= scoreBF
      }
    }
  }
}
