package assigner.model

import assigner._

import java.net.{MalformedURLException, URL}

/**
 * Store success / failure endpoints for post-back.
 * @param success endpoint URL to call on success
 * @param failure endpoint URL to call on failure
 */
case class Endpoints(success: String, failure: String) {

  /**
   * Validate the endpoint URLs.
   * Errors will prevent the algorithm from running.
   * Warnings can be ignored, but are probably faulty input.
   * @return a sequence of any warnings and errors in the data.
   */
  def validate: Validation = {
    val successURL = try { new URL(success); succ() }
      catch { case _: MalformedURLException =>
        err(s"Malformed success endpoint URL: $success")
      }

    val failureURL = try { new URL(failure); succ() }
      catch { case _: MalformedURLException =>
        err(s"Malformed failure endpoint URL: $failure")
      }

    successURL merge failureURL
  }
}
