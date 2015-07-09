package assigner

import assigner.model._

package object service {
  implicit class ValidationJson(val self: Validation) extends AnyVal {
    def jsonMap: Map[String, Any] = self match {
      case Success(message) => Map("success"  -> message)
      case Warning(message) => Map("warnings" -> (message :: Nil))
      case Error  (message) => Map("errors"   -> (message :: Nil))
    }
  }
  
  implicit class SeqValJson(val self: Seq[Validation]) extends AnyVal {
    def jsonMap: Map[String, Any] = {
      val warnings = self collect { case Warning(message) => message }
      val errors   = self collect { case Error  (message) => message }
      if (warnings.isEmpty && errors.isEmpty)
        Map("success" -> self.head.message)
      else
        Map("warnings" -> warnings, "errors" -> errors)
    }
  }
}
