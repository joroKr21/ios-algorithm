package assigner

import org.json4s.jackson.Serialization._
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.test.specs2._

class ServletSpec extends MutableScalatraSpec {

  // Sets up automatic case class to JSON output serialization, required by
  // the JValueResult trait.
  protected implicit val jsonFormats: Formats = DefaultFormats
  addServlet(classOf[Servlet], "/*")

  "POST /run on Servlet with id = 1" should {
    "return status 200 and start the algorithm the first time and says its still running the second" in {
      val course = initPost(1)
      val json: String = write(course)

      post("/run", json) {
        status must_== 200
        body must_== "Algorithm successfully started!"
      }

      post("/run", json) {
        status must_== 200
        body must_== "Algorithm still running!"
      }
    }
  }

  "POST /run on Servlet with id = 2" should {
    "return status 200 and body = Algorithm successfully started!" in {
      val course = initPost(2)
      val json: String = write(course)

      post("/run", json) {
        status must_== 200
        body must_== "Algorithm successfully started!"
      }
    }
  }

  def initPost(id: Int): Course = {
    val students = Set[Student](
      Student(0, "dss", true, Map("1" -> 5, "2" -> 3, "3" -> 4), List(2, 1, 0), Set(4), Set()),
      Student(1, "dss", true, Map("1" -> 1, "2" -> 5, "3" -> 2), List(1, 0, 2), Set(), Set(7)),
      Student(2, "dss", true, Map("1" -> 4, "2" -> 5, "3" -> 3), List(0, 2, 1), Set(), Set()),
      Student(3, "dss", true, Map("1" -> 2, "2" -> 4, "3" -> 5), List(0, 1, 2), Set(1, 2, 4, 5, 6, 7, 8, 9), Set()),
      Student(4, "dss", true, Map("1" -> 4, "2" -> 4, "3" -> 3), List(2, 1, 0), Set(), Set()),
      Student(5, "dss", true, Map("1" -> 3, "2" -> 3, "3" -> 3), List(1, 2, 0), Set(3, 8), Set()),
      Student(6, "dss", true, Map("1" -> 1, "2" -> 4, "3" -> 3), List(0, 1, 2), Set(), Set()),
      Student(7, "dss", true, Map("1" -> 4, "2" -> 2, "3" -> 5), List(1, 2, 0), Set(), Set()),
      Student(8, "dss", true, Map("1" -> 1, "2" -> 2, "3" -> 1), List(2, 0, 1), Set(), Set(2)),
      Student(9, "dss", true, Map("1" -> 3, "2" -> 4, "3" -> 2), List(0, 2, 1), Set(), Set())
    ).map(s => s.id -> s).toMap

    val groups = Set[Group](
      Group(0, 3, 3, Set("1", "2", "3")),
      Group(1, 3, 3, Set("1", "2", "3")),
      Group(2, 3, 3, Set("1", "2", "3"))
    ).map(s => s.id -> s).toMap

    val settings = Settings(diverse = true, iterations = 20)

    Course(id, settings, students.values.toList, groups.values.toList, Set("1", "2", "3"))
  }
}
