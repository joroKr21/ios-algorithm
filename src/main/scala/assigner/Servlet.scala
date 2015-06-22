package assigner

import org.json4s._
import org.json4s.{DefaultFormats, Formats}
import org.json4s.jackson.Serialization._
import org.scalatra.ScalatraServlet
import org.scalatra.json.JacksonJsonSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scalaj.http.Http

class Servlet extends ScalatraServlet with JacksonJsonSupport {

  // Sets up automatic case class to JSON output serialization, required by
  // the JValueResult trait.
  protected implicit val jsonFormats: Formats = DefaultFormats

  // A map in memory where we save all the jobs we run
  val courseMap = scala.collection.mutable.Map[Int, Boolean]()

  // Before every action runs, set the content type to be in JSON format.
  before() {
    contentType = formats("json")
  }

  get("/finished/:courseId") {
    val courseId = params("courseId").toInt
    logger.info(s"Requested status of course $courseId")
    if (courseMap.contains(courseId)) {
        if (courseMap(courseId)) "Algorithm is finished"
        else "Algorithm is still running"
    } else {
      "Course is not known"
    }
  }

  post("/run") {
    logger.info(s"Got request headers: ${request.headers}")
    logger.info(s"Got request body: ${request.body}")
    val input = parsedBody.extract[Course]
    val endpoints = input.endpoints
    val courseId: Int = input.courseId
    if (courseMap.contains(courseId) && !courseMap(courseId)) {
      "Algorithm is still running"
    } else {
      courseMap(courseId) = false

      // Run the algorithm asynchronously
      val f: Future[Assignment] = Future {
        val assigner = new Assigner(input)

        // For testing purposes
        Thread.sleep(1000)

        assigner.startSolving()
      }

      // Callback when the algorithm is finished async
      f onComplete {
        case Success(assignment) =>
          courseMap(courseId) = true
          logger.info(s"Best solution: $assignment")
          val data = write(Map("courseId"   -> courseId,
                               "studentMap" -> assignment.studentMap,
                               "groupMap"   -> assignment.groupMap))
          val code = Http(endpoints.success).postData(data).header("content-type", "application/json").asString.code
          logger.info("Status: " + code)

        case Failure(t) =>
          logger.error(t.getMessage)
          logger.error(t.getStackTrace.mkString("\n"))
          val code = Http(endpoints.failure).postData("Something went terribly wrong").asString.code
          logger.info("Status " + code)
      }

      "Algorithm successfully started"
    }
  }

  post("/postback") {
    println(parsedBody.toString)
  }
}
