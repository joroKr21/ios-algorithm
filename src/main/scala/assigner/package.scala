import org.coinor.opents.SolutionAdapter

import scala.util.Random

package object assigner {

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

}