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

  get("/finished:courseId") {
    val courseId = params("courseId").toInt
    if(courseMap.contains(courseId)) {
      val output =
        if (courseMap(courseId)) "Finished"
        else "Running"

      output
    } else {
      "Course is not known"
    }
  }

  post("/run") {
    val input = parsedBody.extract[Course]
    val courseId: Int = input.courseId
    if(courseMap.contains(courseId) && !courseMap(courseId)){
      "Algorithm still running!"
    } else {
      courseMap(courseId) = false

      // Run the algorithm asynchronously
      val f: Future[Assignment] = Future {
        val assigner = new Assigner(input)

        // For testing purposes
        Thread.sleep(1000)

        assigner.startSolving()
      }


      f onComplete {
        case Success(assignment) =>
          courseMap(courseId) = true
          logger.info("Best Solution")
          val data: String = write(Map("Student Map" -> assignment.studentMap, "Group Map" -> assignment.groupMap))
          logger.info(data)

          // TODO: Post the result of the algorithm to the backend

        case Failure(t) =>
          println("An error has occurred: " + t.getMessage)

          // TODO: Post the failure message to the backend and save the logs to a file or database

      }

      "Algorithm successfully started!"
    }
  }
}
