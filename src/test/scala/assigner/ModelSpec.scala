package assigner

import assigner.model._

import org.junit.runner.RunWith
import org.scalacheck.Gen._
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[JUnitRunner])
class ModelSpec extends FlatSpec with Matchers with PropertyChecks with DataGen {

  val epsilon = 1e-7
  val courses = courseGen(
    settings       = Settings(),
    endpoints      = Endpoints("http://localhost", "http://localhost"),
    numStudentsGen = choose(20, 30),
    numGroupsGen   = const(5),
    numSkillsGen   = choose(2, 3),
    groupSizeGen   = minMaxGroupSizeGen(4, 6))

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(workers = 2)

  "a course" should "be able to normalize all weights" in {
    forAll(courses, workers(2)) { course: Course =>
      val scale      = 10.0
      val normalized = course normalized scale
      normalized.weights.values.sum should be (scale +- epsilon)
      all(normalized.students.map { _.weights.values.sum }) should be (scale +- epsilon)
    }
  }
}