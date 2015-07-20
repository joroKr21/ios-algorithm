package assigner.service

import akka.actor.ActorSystem
import akka.io.IO

import assigner._
import assigner.model._
import assigner.search._
import assigner.search.moves._

import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization._

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

import spray.can.Http
import spray.http._
import spray.http.HttpMethods._
import spray.http.MediaTypes._
import spray.httpx.Json4sJacksonSupport
import spray.routing._
import spray.util.LoggingContext

object SprayService extends App with SimpleRoutingApp with Json4sJacksonSupport {

  // ExecutionContext
  implicit val system = ActorSystem("simple-routing-app")
  import system.dispatcher

  // logging
  val log = implicitly[LoggingContext]
  // JSON Marshalling
  val json4sJacksonFormats = DefaultFormats
  // concurrent Map for tracking running jobs
  val running = TrieMap.empty[Long, Boolean] withDefaultValue false
  // get the port
  val port = if (args.nonEmpty) args.head.toInt else 8080

  /**
   * Complete an HTTP response with a raw [[String]] message (as opposed to marshalling the
   * response object.
   * @param message The raw HTTP response body
   * @return a [[StandardRoute]] that completes the HTTP response
   */
  def raw(message: String): StandardRoute =
    complete(HttpResponse(entity = message))

  /** Echo the HTTP request entity back as a [[String]]. */
  val echo = path("echo") {
    post {
      extract(_.request.entity.asString) { message =>
        log.debug("Repeating message {}", message)
        raw(message)
      }
    }
  }

  /** Get the status of a job by its ID (is it running or not). */
  val status = path("finished" / LongNumber) { id =>
    get {
      log.debug("Requested status of job {}", id)
      if (running(id)) raw(s"Job $id is running.")
      else raw(s"Job $id is not running.")
    }
  }

  /** Evaluate an assignment. */
  def score = path("score") {
    post {
      detach() {
        entity(as[CourseWithAssignment]) { cwa =>
          val course = cwa.course
          val studentMap = cwa.studentMap.sorted
          val groupMap = cwa.groupMap.mapValues { _.sorted }.sorted
          val assignment = Assignment(studentMap, groupMap)
          Map("score" -> new Objective(course).score(assignment))
        }
      }
    }
  }

  /** Run a job. */
  val run = path("run") {
    post {
      entity(as[Course]) { course =>
        val id = course.id
        log.debug("Requested to run job {}", id)
        if (running(id)) { // already running
          val message = s"Job $id already running"
          log warning message
          raw(message)
        } else { // actually do the work
          running += id -> true
          val message = s"Running job $id"
          log info message
          Future { // spawn off the heavy lifting asynchronously
            implicit val formats = json4sJacksonFormats
            val Endpoints(success, failure) = course.endpoints
            val validation = course.validate
            if (validation.errors.nonEmpty) {
              log.error("Errors trying to run job {}", id)
              validation.errors foreach log.error
              // fire and forget
              // TODO: check the response (via akka ask pattern)
              val entity = HttpEntity(`application/json`, write(validation))
              IO(Http) ! HttpRequest(POST, failure, entity = entity)
            } else {
              if (validation.warnings.nonEmpty) {
                log.warning("Warnings while running job {}", id)
                validation.warnings foreach log.warning
              }

              val assigner = new Assigner(course)
              val (initial, moves, assignment) = assigner.solution

              val movesLog = moves collect {
                case (Swap(s1, s2), score) =>
                  Map("move" -> "swap", "student1" -> s1, "student2" -> s2, "score" -> score)
                case (Switch(s, g), score) =>
                  Map("move" -> "switch", "student" -> s, "group" -> g, "score" -> score)
                case (DropGroup(g), score) =>
                  Map("move" -> "drop", "group" -> g, "score" -> score)
                case (FillGroup(g, ss), score) =>
                  Map("move" -> "fill", "group" -> g, "students" -> ss, "score" -> score)
              }

              val result   = Map(
                "initialStudentMap" -> initial.studentMap,
                "initialGroupMap"   -> initial.groupMap,
                "movesLog"          -> movesLog,
                "studentMap"        -> assignment.studentMap,
                "groupMap"          -> assignment.groupMap)

              // fire and forget
              // TODO: check the response (via akka ask pattern)
              IO(Http) ! HttpRequest(POST, success, entity = write(result))
            }

            // done running this instance
            running -= id
          }

          // reply right away
          raw(message)
        }
      }
    }
  }

  startServer(interface = "localhost", port = port) {
    echo ~ status ~ run
  }
}
