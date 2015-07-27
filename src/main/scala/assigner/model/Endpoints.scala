package assigner.model

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
    def validateURL(url: String, msg: String) =
      try   { new URL(url); succ() }
      catch { case _: MalformedURLException => err(msg) }

    val successURL = validateURL(success, s"Malformed success endpoint URL: $success")
    val failureURL = validateURL(failure, s"Malformed failure endpoint URL: $failure")
    successURL merge failureURL
  }

  /**
   * Prepend "http://host:port" in front of any relative URLs contained in this object.
   * @param host the host name or IP address to prepend (default: "localhost")
   * @param port the HTTP port number to prepend (default: 8080)
   * @return a new set of endpoints with fully qualified URLs
   */
  def qualify(host: String = default.host, port: Int = default.port): Endpoints = {
    def qualifyURL(url: String) = try { new URL(url).toString }
      catch { case _: MalformedURLException =>
        s"http://$host:$port/${url.dropWhile(_ == '/')}"
      }

    Endpoints(qualifyURL(success), qualifyURL(failure))
  }
}
