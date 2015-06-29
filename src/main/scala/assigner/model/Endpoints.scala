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
   * [[Error]]s will prevent the algorithm from running.
   * [[Warning]]s can be ignored, but are probably faulty input.
   * @return a sequence of any [[Warning]]s and [[Error]]s in the data.
   */
  def validate: Seq[Validation] = {
    val successURL = try { new URL(success); Nil }
      catch { case _: MalformedURLException =>
        err(s"Malformed success endpoint URL: $success").sng
      }

    val failureURL = try { new URL(failure); Nil }
      catch { case _: MalformedURLException =>
        err(s"Malformed failure endpoint URL: $failure").sng
      }

    flatten(successURL, failureURL)
  }
}
