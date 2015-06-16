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
        body must_== "Algorithm successfully started"
      }

      post("/run", json) {
        status must_== 200
        body must_== "Algorithm is still running"
      }

      get("/finished/1") {
        status must_== 200
        body must_== "Algorithm is still running"
      }
    }
  }

  "POST /run on Servlet with id = 2" should {
    "return status 200 and body = Algorithm successfully started!" in {
      val course = initPost(2)
      val json: String = write(course)

      post("/run", json) {
        status must_== 200
        body must_== "Algorithm successfully started"
      }
    }
  }

  "Get /finished/3" should {
    "return status 200 say the course is not known" in {
      get("/finished/3") {
        status must_== 200
        body must_== "Course is not known"
      }
    }
  }

  def initPost(id: Int): Course = {
    val students = Set[Student](
      Student(id = 0, name = "Student 0", mandatory = true, skills = Map("1" -> 5, "2" -> 3, "3" -> 4),
        preferences = List(2, 1, 0), friends = Set(4)),
      Student(id = 1, name = "Student 1", mandatory = true, skills = Map("1" -> 1, "2" -> 5, "3" -> 2),
        preferences = List(1, 0, 2), friends = Set()),
      Student(id = 2, name = "Student 2", mandatory = true, skills = Map("1" -> 4, "2" -> 5, "3" -> 3),
        preferences = List(0, 2, 1), friends = Set()),
      Student(id = 3, name = "Student 3", mandatory = true, skills = Map("1" -> 2, "2" -> 4, "3" -> 5),
        preferences = List(0, 1, 2), friends = Set(1,2,4,5,6,7,8,9)),
      Student(id = 4, name = "Student 4", mandatory = true, skills = Map("1" -> 4, "2" -> 4, "3" -> 3),
        preferences = List(2, 1, 0), friends = Set()),
      Student(id = 5, name = "Student 5", mandatory = true, skills = Map("1" -> 3, "2" -> 3, "3" -> 3),
        preferences = List(1, 2, 0), friends = Set(3, 8)),
      Student(id = 6, name = "Student 6", mandatory = true, skills = Map("1" -> 1, "2" -> 4, "3" -> 3),
        preferences = List(0, 1, 2), friends = Set()),
      Student(id = 7, name = "Student 7", mandatory = true, skills = Map("1" -> 4, "2" -> 2, "3" -> 5),
        preferences = List(1, 2, 0), friends = Set()),
      Student(id = 8, name = "Student 8", mandatory = true, skills = Map("1" -> 1, "2" -> 2, "3" -> 1),
        preferences = List(2, 0, 1), friends = Set()),
      Student(id = 9, name = "Student 9", mandatory = true, skills = Map("1" -> 3, "2" -> 4, "3" -> 2),
        preferences = List(0, 2, 1), friends = Set())
    ).map(s => s.id -> s).toMap

    val groups = Set[Group](
      Group(id = 0, minSize = 0, maxSize = 3, name = "Group 0", skills = Set("1", "2", "3")),
      Group(id = 1, minSize = 3, maxSize = 3, name = "Group 1", skills = Set("1", "2", "3")),
      Group(id = 2, minSize = 3, maxSize = 3, name = "Group 2", skills = Set("1", "2", "3"))
    ).map(s => s.id -> s).toMap

    val settings = Settings(diverse = true, iterations = 20)

    Course(id, settings, students.values.toList, groups.values.toList, Set("1", "2", "3"))
  }
}
