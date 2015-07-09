package assigner.service

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext

/** Embedded Jetty launcher. */
object JettyLauncher extends App {
  // this is my entry object as specified in sbt project definition
  val port = if (System.getenv("PORT") != null)
    System.getenv("PORT").toInt else 8080

  val server  = new Server(port)
  val context = new WebAppContext()

  context.setContextPath("/")
  context.setResourceBase("src/main/webapp")
  context.addServlet(classOf[Servlet], "/")
  server.setHandler(context)
  server.start()
  server.join()
}
