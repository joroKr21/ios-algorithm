package assigner.service

import assigner.Merge
import assigner.model._
import assigner.search._

import org.json4s._
import org.json4s.jackson.Serialization._
import org.scalatra.ScalatraServlet
import org.scalatra.json.JacksonJsonSupport
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scalaj.http.Http

class Servlet extends ScalatraServlet with JacksonJsonSupport {

  lazy val logger = LoggerFactory getLogger getClass

  // Sets up automatic case class to JSON output serialization, required by
  // the JValueResult trait.
  protected implicit val jsonFormats: Formats = DefaultFormats

  // A map in memory where we save all the jobs we run
  val courseMap = mutable.Map.empty[CourseId, Boolean]

  // Before every action runs, set the content type to be in JSON format.
  before() {
    contentType = formats("json")
  }

  get("/finished/:jobId") {
    val id = params("jobId").toLong
    logger.info(s"Requested status for job $id")
    if (courseMap contains id) {
      if (courseMap(id)) succ("Job finished"     ).jsonMap merge jobId(id)
      else               succ("Job still running").jsonMap merge jobId(id)
    } else                err("Job unknonw"      ).jsonMap merge jobId(id)
  }

  post("/run") {
    logger.info(s"Requested to run job:\n${request.body}")
    val course     = parsedBody.extract[Course]
    val validation = course.validate.jsonMap
    if (validation contains "errors") validation else {
      val endpoints = course.endpoints
      val id        = course.id
      
      if (courseMap.contains(id) && !courseMap(id)) {
        err("Job still running").jsonMap merge jobId(id)
      } else {
        courseMap(id) = false
        // run the algorithm asynchronously
        val future: Future[Assignment] = Future {
          new Assigner(course).solution
        }

        // callback when the algorithm is finished async
        future onComplete {
          case Success(assignment) =>
            courseMap(id) = true
            logger.info(s"Best solution found for job $id: $assignment")
            val data = write {
              succ("Best solution found").jsonMap merge jobId(id) merge Map(
                "studentMap" -> assignment.studentMap,
                "groupMap"   -> assignment.groupMap)
            }
            
            val http = Http(endpoints.success).postData(data)
              .header("content-type", "application/json").asString
            
            logger.info(s"Response for job $id: ${http.code}, ${http.body}")

          case Failure(t) =>
            logger.error(s"Job $id failed")
            logger.error(t.getMessage)
            logger.error(t.getStackTrace mkString "\n")
            
            val data = write {
              err(t.getMessage).jsonMap merge jobId(id)
            }
            
            val http = Http(endpoints.failure).postData(data).asString
            logger.info(s"Response for job $id: ${http.code}, ${http.body}")
        }

        succ("Job successfully started").jsonMap merge
          jobId(id) merge validation
      }
    }
  }

  post("/echo") {
    val message = parsedBody.toString
    logger.info(s"Repeating $message")
    message
  }

  private def jobId(id: CourseId) =
    Map("jobId" -> id)
}
