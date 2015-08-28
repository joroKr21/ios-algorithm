package assigner.service

import akka.actor.ActorSystem
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout

import assigner._
import assigner.model._
import assigner.search._
import assigner.search.moves._

import org.json4s.{Formats, DefaultFormats}
import org.json4s.jackson.Serialization._

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Failure, Success}

import spray.can.Http
import spray.http._
import spray.http.HttpHeaders._
import spray.http.HttpMethods._
import spray.http.MediaTypes._
import spray.httpx.Json4sJacksonSupport
import spray.routing._
import spray.util.LoggingContext

object SprayService extends App with SimpleRoutingApp with Json4sJacksonSupport {

  // ExecutionContext
  implicit val system  = ActorSystem("simple-routing-app")
  implicit val timeout = Timeout(5.seconds)
  import system.dispatcher

  // logging
  val log = implicitly[LoggingContext]
  // JSON Marshalling
  implicit val json4sJacksonFormats = DefaultFormats
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

  /**
   * Reply to the specified URL with the specified answer and log the response.
   * @param answer Reply object to be serialized as JSON
   */
  def reply[A <: AnyRef](url: String, answer: A)(implicit formats: Formats): Unit = {
    val headers = `Content-Type`(`application/json`) :: Nil
    val entity  = HttpEntity(`application/json`, write(answer))
    IO(Http) ? HttpRequest(POST, url, headers, entity) onComplete {
      case Success(_) => log.debug("Successfully responded to {}", url)
      case Failure(e) => log.error(e, "Error responding to {}",    url)
    }
  }

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
  val score = path("score") {
    post {
      detach() {
        entity(as[CourseWithAssignment]) { cwa =>
          val id = cwa.course.id
          log.debug("Calculating score of assignment {}", id)
          val validation = cwa.validate

          if (validation.warnings.nonEmpty) { // log warnings
            log.warning("Warnings while calculating score of assignment {}", id)
            validation.warnings foreach log.warning
          }

          if (validation.errors.nonEmpty) { // log errors
            log.error("Errors while calculating score of assignment {}", id)
            validation.errors foreach log.error
            complete(validation)
          } else {
            val groupMap   = cwa.groupMap.mapValues { _.sorted }.sorted
            val studentMap = (for ((g, ss) <- groupMap; s <- ss) yield s -> g).toMap.sorted
            val assignment = Assignment(studentMap, groupMap)
            complete(Map("score" -> new Objective(cwa.course).score(assignment)))
          }
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
            running    += id -> true
            val message = s"Running job $id"
            log info message
            val Endpoints(success, failure) =
              course.endpoints.qualify(host = ip.toString())

            Future { // spawn off the heavy lifting asynchronously
              val validation = course.validate

              if (validation.warnings.nonEmpty) { // log warnings
                log.warning("Warnings while running job {}", id)
                validation.warnings foreach log.warning
              }

              if (validation.errors.nonEmpty) { // log errors
                log.error("Errors trying to run job {}", id)
                validation.errors foreach log.error
                reply(failure, validation)
              } else {
                val assigner = new Assigner(course)
                val (initial, moves, assignment) = assigner.solution

                val movesLog = moves collect {
                  case (Swap(s1, s2), obj) =>
                    Map("move" -> "swap", "student1" -> s1, "student2" -> s2, "score" -> obj)
                  case (Switch(s, g), obj) =>
                    Map("move" -> "switch", "student" -> s, "group" -> g, "score" -> obj)
                  case (DropGroup(g), obj) =>
                    Map("move" -> "drop", "group" -> g, "score" -> obj)
                  case (FillGroup(g, ss), obj) =>
                    Map("move" -> "fill", "group" -> g, "students" -> ss, "score" -> obj)
                }

                reply(success, Map(
                  "initialStudentMap" -> initial.studentMap,
                  "initialGroupMap"   -> initial.groupMap,
                  "movesLog"          -> movesLog,
                  "studentMap"        -> assignment.studentMap,
                  "groupMap"          -> assignment.groupMap))
              }

              running -= id // done running this instance
            } onFailure { case error: Throwable =>
              log.error(error, "Error while running job {}", id)
              reply(failure, Validation(errors = error.getMessage :: Nil))
            }

            raw(message) // reply right away
          }
        }
      }
    }
  }

  /** Test a different assignment. */
  val swap = path("swap") {
    post {
      clientIP { ip =>
        log.debug("Accepted request by {}", ip)
        entity(as[CourseWithAssignment]) { cwa =>
          val course     = cwa.course
          val id         = course.id
          val groupMap   = cwa.groupMap.mapValues { _.sorted }.sorted
          val studentMap = (for ((g, ss) <- groupMap; s <- ss) yield s -> g).toMap.sorted
          val assignment = Assignment(studentMap, groupMap)
          log.debug("Requested to fill up empty spots for job {}", id)
          if (running(id)) { // job is still running
            val message = s"Job $id is still running"
            log warning message
            raw(message)
          } else { // actually do the work
            val message = s"Filling up empty spots for job $id"
            log info message
            val Endpoints(success, failure) =
              course.endpoints.qualify(host = ip.toString())

            Future { // spawn off the heavy lifting asynchronously
              val validation = course.validate

              if (validation.warnings.nonEmpty) { // log warnings
                log.warning("Warnings while filling up the empty spots for job {}", id)
                validation.warnings foreach log.warning
              }

              if (validation.errors.nonEmpty) { // log errors
                log.error("Errors trying to fill up the empty spots for job {}", id)
                validation.errors foreach log.error
                reply(failure, validation)
              } else {
                val assigner                   = new Assigner(course)
                val (initial, moves, solution) = assigner swap assignment

                val movesLog = moves collect {
                  case (Swap(s1, s2), obj) =>
                    Map("move" -> "swap", "student1" -> s1, "student2" -> s2, "score" -> obj)
                  case (Switch(s, g), obj) =>
                    Map("move" -> "switch", "student" -> s, "group" -> g, "score" -> obj)
                  case (DropGroup(g), obj) =>
                    Map("move" -> "drop", "group" -> g, "score" -> obj)
                  case (FillGroup(g, ss), obj) =>
                    Map("move" -> "fill", "group" -> g, "students" -> ss, "score" -> obj)
                }

                reply(success, Map(
                  "initialStudentMap" -> initial.studentMap,
                  "initialGroupMap"   -> initial.groupMap,
                  "movesLog"          -> movesLog,
                  "studentMap"        -> solution.studentMap,
                  "groupMap"          -> solution.groupMap))
              }

              running -= id // done running this instance
            } onFailure { case t: Throwable =>
              log.error(t, "Error while filling up the empty spots for job {}", id)
              reply(failure, Validation(errors = t.getMessage :: Nil))
            }

            raw(message) // reply right away
          }
        }
      }
    }
  }

  startServer(interface = default.host, port = port) {
    echo ~ status ~ score ~ run ~ swap
  }
}
