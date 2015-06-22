import org.coinor.opents.SolutionAdapter
import org.slf4j.LoggerFactory

import scala.util.Random

package object assigner {

  def logger = LoggerFactory.getLogger(this.getClass)

  case class Student(id: Int,
                     name: String = "",
                     mandatory: Boolean = false,
                     skills: Map[String, Int] = Map.empty,
                     weights: Map[String, Double] = Map.empty,
                     preferences: List[Int] = Nil,
                     friends: Set[Int] = Set.empty,
                     foes: Set[Int] = Set.empty)

  case class Group(id: Int,
                   minSize: Int,
                   maxSize: Int,
                   name: String = "",
                   skills: Set[String] = Set.empty)

  case class Endpoints(success: String, failure: String)

  case class Settings(iterations: Int,
                      diverse: Boolean = true,
                      weights: Map[String, Double] = Map.empty)

  case class Course(courseId: Int,
                    settings: Settings,
                    students: List[Student],
                    groups: List[Group],
                    skills: Set[String] = Set.empty,
                    weights: Map[String, Double] = Map.empty,
                    endpoints: Endpoints = null) {
    def studentMap = students.map { s => s.id -> s }.toMap

    def groupMap = groups.map { g => g.id -> g }.toMap

    def hasGlobalWeights = weights.nonEmpty
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
      def sqr(x: A) = x * x
      val m = self.mean
      self.map { x => sqr(x - m) }.mean
    }
  }

  implicit class MapOps[K, V](val self: Map[K, V]) extends AnyVal {
    def zipMap[V2](that: Map[K, V2]) =
      (self.keySet intersect that.keySet).map { key =>
        key ->(self(key), that(key))
      }.toMap

    def merge(that: Map[K, V]) =
      (self.keySet union that.keySet).map { key =>
        key -> that.getOrElse(key, self(key))
      }.toMap
  }

}