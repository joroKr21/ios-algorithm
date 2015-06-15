import org.coinor.opents.SolutionAdapter
import org.slf4j.LoggerFactory

import scala.util.Random
import scalaj.http.Http

package object assigner {

  def logger = LoggerFactory.getLogger(this.getClass)

  case class Student(id: Int,
                     name: String,
                     mandatory: Boolean = false,
                     skills: Map[String, Int],
                     groupPreferences: List[Int] = Nil,
                     friends: Set[Int] = Set.empty,
                     foes: Set[Int] = Set.empty)

  case class Group(id: Int,
                   minSize: Int,
                   maxSize: Int,
                   skills: Set[String] = Set.empty)

  case class Settings(diverse: Boolean, iterations: Int)

  case class Course(courseId: Int,
                    settings: Settings,
                    students: List[Student],
                    groups: List[Group],
                    skills: Set[String] = Set.empty) {
    def hasQueue = students.size > groups.map(_.maxSize).sum
  }

  case class Assignment(var studentMap: Map[Int, Int],
                        var groupMap: Map[Int, Set[Int]])
      extends SolutionAdapter {
    def copy = clone

    override def clone = {
      val copy = super.clone.asInstanceOf[Assignment]
      copy.studentMap = studentMap
      copy.groupMap = groupMap
      copy
    }
  }

  implicit class Shuffle[A](val self: List[A]) extends AnyVal {
    def shuffle = Random shuffle self
  }

  implicit class MeanVar[A: Fractional](self: Traversable[A]) {
    import Fractional.Implicits._

    def mean = {
      val f = implicitly[Fractional[A]]
      self.sum / f.fromInt(self.size)
    }

    def variance = {
      val m = self.mean
      self.map { _ - m }.map { x => x * x }.mean
    }
  }

  def post(url: String, data: String) =
    Http(url)
      .postData(data)
      .header("content-type", "application/json")
      .asString
      .code

//  def toJSON(obj: Any) : String =  write(obj)

}