package assigner

import org.json4s.jackson.Serialization._
import org.json4s.{DefaultFormats, Formats, _}
import org.scalatra.ScalatraServlet
import org.scalatra.json.JacksonJsonSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class Servlet extends ScalatraServlet with JacksonJsonSupport {

  // Sets up automatic case class to JSON output serialization, required by
  // the JValueResult trait.
  protected implicit val jsonFormats: Formats = DefaultFormats

  val courseMap = scala.collection.mutable.Map[Int, Boolean]()

  // Before every action runs, set the content type to be in JSON format.
  before() {
    contentType = formats("json")
  }

  post("/run") {
    val input = parsedBody.extract[Course]
    val courseId: Int = input.courseId
    if(courseMap.contains(courseId) && !courseMap(courseId)){
      "Job still running"
    } else {
      courseMap(courseId) = false

      // Run the algorithm asynchronously
      val f: Future[Assignment] = Future {
        val assigner = new Assigner(input)

        assigner.startSolving()
      }

      f onComplete {
        case Success(assignment) =>
          logger.info("Best Solution")
          val data: String = write(Map("Student Map" -> assignment.studentMap, "Group Map" -> assignment.groupMap))
          logger.info(data)

          // TODO: Post the result of the algorithm to the backend
        case Failure(t) =>
          println("An error has occured: " + t.getMessage)

          // TODO: Post the failure message to the backend and save the logs to a file or database
      }
    }
  }

  // endpoint for testing purposes
  get("/") {
    val students = Set[Student](
      Student(0, "dss", true, Map("1" -> 5, "2" -> 3, "3" -> 4), List(2, 1, 0), Set(4), Set()),
      Student(1, "dss", true, Map("1" -> 1, "2" -> 5, "3" -> 2), List(1, 0, 2), Set(), Set(7)),
      Student(2, "dss", true, Map("1" -> 4, "2" -> 5, "3" -> 3), List(0, 2, 1), Set(), Set()),
      Student(3, "dss", true, Map("1" -> 2, "2" -> 4, "3" -> 5), List(0, 1, 2), Set(1,2,4,5,6,7,8,9), Set()),
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
    val courseId = 1
    val input = Course(courseId, settings, students.values.toList, groups.values.toList, Set("1", "2", "3"))

    if(courseMap.contains(courseId) && !courseMap(courseId)){
      "Job still running"
    } else {
      courseMap(courseId) = false

      // Run the algorithm asynchronously
      val f: Future[Assignment] = Future {
        val assigner = new Assigner(input)

        Thread.sleep(10000)
        assigner.startSolving()
      }

      f onComplete {
        case Success(assignment) =>
          logger.info("Best Solution")
          val data: String = write(Map("Student Map" -> assignment.studentMap, "Group Map" -> assignment.groupMap))
          logger.info(data)

        // TODO: Post the result of the algorithm to the backend
        case Failure(t) =>
          println("An error has occured: " + t.getMessage)

        // TODO: Post the failure message to the backend and save the logs to a file or database
      }

      "Algorithm successfully started!"
    }

  }
}
