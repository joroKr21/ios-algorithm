import org.coinor.opents.SolutionAdapter
import org.slf4j.LoggerFactory

import scala.util.Random
import scalaj.http.Http

package object assigner {

  def logger = LoggerFactory.getLogger(this.getClass)

  case class Student(id: Int,
                     major: String,
                     mandatory: Boolean,
                     skills: Map[String, Int],
                     preferences: List[Int],
                     friends: Set[Int],
                     foes: Set[Int])

  case class Group(id: Int,
                   minSize: Int,
                   maxSize: Int,
                   skills: Set[String])

  case class Settings(diverse: Boolean, numStartPoints: Int, numIterations: Int)

  case class Input(courseId: Int, settings: Settings, students: Set[Student], groups: Set[Group])

  case class Assignment(var studentMap: Map[Int, Int],
                        var groupMap: Map[Int, Set[Int]])
    extends SolutionAdapter {

    override def clone = {
      val copy: Assignment = super.clone.asInstanceOf[Assignment]
      copy.studentMap = studentMap
      copy.groupMap = groupMap

      copy
    }

    def copy() = clone()
  }

  implicit class RandomShuffle(val list: List[Int]) {
    def shuffle = Random.shuffle(list)
  }

  def post(url: String, data: String) =
    Http(url)
      .postData(data)
      .header("content-type", "application/json")
      .asString
      .code

//  def toJSON(obj: Any) : String =  write(obj)

}