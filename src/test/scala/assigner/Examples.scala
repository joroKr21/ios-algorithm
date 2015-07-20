package assigner

import assigner.model._
import org.json4s.DefaultFormats
import org.junit.runner.RunWith
import org.scalacheck.Gen._
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks
import org.json4s.jackson.Serialization._

@RunWith(classOf[JUnitRunner])
class Examples extends FunSuite with Matchers with PropertyChecks with DataGen {
  implicit val formats = DefaultFormats

  val depth   = 3
  val courses = courseGen(
    settings       = Settings(),
    endpoints      = Endpoints("http://localhost:8080/echo", "http://localhost:8080/echo"),
    numStudentsGen = choose(20, 30),
    numGroupsGen   = const(5),
    numSkillsGen   = choose(2, 3),
    groupSizeGen   = minMaxGroupSizeGen(4, 6))

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 10, workers = 2)

  test("positive examples") {
    forAll(courses) { course: Course =>
      whenever(course.validate.errors.isEmpty) {
        println(write(course))
      }
    }
  }
}
