package assigner.service

import akka.actor.ActorSystem
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout

import assigner._
import assigner.model._
import assigner.search._
import assigner.search.moves._

import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization._

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Failure, Success}

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
  val port = if (args.nonEmpty) args.head.toInt else default.port

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
      clientIP { ip =>
        log.debug("Accepted request by {}", ip)
        entity(as[Course]) { course =>
          val id = course.id
          log.debug("Requested to run job {}", id)
          if (running(id)) { // already running
            val message = s"Job $id already running"
            log warning message
            raw(message)
          } else { // actually do the work
            implicit val formats = json4sJacksonFormats
            implicit val timeout = Timeout(5.seconds)
            running    += id -> true
            val message = s"Running job $id"
            log info message
            val Endpoints(success, failure) =
              course.endpoints.qualify(host = ip.toString())

            Future { // spawn off the heavy lifting asynchronously
              val validation = course.validate
              if (validation.errors.nonEmpty) {
                log.error("Errors trying to run job {}", id)
                validation.errors foreach log.error
                val entity = HttpEntity(`application/json`, write(validation))
                IO(Http) ? HttpRequest(POST, failure, entity = entity) onComplete {
                  case Success(_) => log.debug("Successfully responded to {}", failure)
                  case Failure(t) => log.error(t, "Error responding to {}",    failure)
                }
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

                val result = Map(
                  "initialStudentMap" -> initial.studentMap,
                  "initialGroupMap"   -> initial.groupMap,
                  "movesLog"          -> movesLog,
                  "studentMap"        -> assignment.studentMap,
                  "groupMap"          -> assignment.groupMap)

                IO(Http) ? HttpRequest(POST, success, entity = write(result)) onComplete {
                  case Success(_) => log.debug("Successfully responded to {}", success)
                  case Failure(e) => log.error(e, "Error responding to {}",    success)
                }
              }

              running -= id // done running this instance
            } onFailure { case t: Throwable =>
              log.error(t, "Error while running job {}", id)
              val message = write(Validation(errors = t.getMessage :: Nil))
              val entity  = HttpEntity(`application/json`, message)
              IO(Http) ? HttpRequest(POST, failure, entity = entity) onComplete {
                case Success(_) => log.debug("Successfully responded to {}", failure)
                case Failure(e) => log.error(e, "Error responding to {}",    failure)
              }
            }

            raw(message) // reply right away
          }
        }
      }
    }
  }

  startServer(interface = default.host, port = port) {
    echo ~ status ~ run
  }
}
