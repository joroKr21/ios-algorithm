package assigner

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[JUnitRunner])
class ImplicitSpec extends FlatSpec with Matchers with PropertyChecks {

  val epsilon = 1e-10

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(workers = 2)

  "flatten" should "yield the original elements" in {
    forAll { xss: Seq[Seq[Int]] =>
      val flat = flatten(xss: _*)
      flat should have size xss.sumBy { _.size }
      val set  = flat.toSet
      for (xs <- xss; x <- xs) set should contain (x)
    }
  }

  it should "preserve the original order" in {
    forAll { xss: Seq[Seq[Int]] =>
      val flat = flatten(xss: _*)
      var pos  = 0
      for (xs <- xss) {
        flat.slice(pos, pos + xs.size) should equal (xs)
        pos += xs.size
      }
    }
  }

  "sng" should "return a List with one element" in {
    val x = 42
    val singleton = x.sng
    singleton should have size 1
    singleton.head should be (x)
  }

  "freq" should "count the occurrences of each element" in {
    forAll { xs: Seq[Int] =>
      val freq = xs.freq
      for ((x, n) <- freq) n should be (xs count { _ == x })
    }
  }

  "freqBy" should "count the occurrences of each key" in {
    forAll { ss: Seq[String] =>
      val freq = ss freqBy { _.length }
      for ((l, n) <- freq) n should be (ss count { _.length == l })
    }
  }

  "mean" should "find the middle of a range" in {
    forAll { start: Double =>
      val end = start + 50
      val act = (start to end by 1).mean
      val exp = start + 25
      act / exp should be (1.0 +- epsilon)
    }
  }

  "sumBy" should "add all elements with a specified key function" in {
    forAll { ss: Seq[String] =>
      var sum = ss sumBy { _.length }
      for (s <- ss) sum -= s.length
      sum should be (0)
    }
  }

  "productBy" should "multiply all elements with a specified key function" in {
    forAll { ss: Seq[String] =>
      whenever(ss forall { _.nonEmpty }) {
        var product = ss productBy { _.length.toDouble }
        for (s <- ss) product /= s.length
        product should be (1.0 +- epsilon)
      }
    }
  }

  "shuffle" should "yield an arbitrary List when called multiple times" in {
    forAll { xs: List[Int] =>
      whenever(xs.distinct.size > 3) {
        val ys = xs.shuffle
        val zs = ys.shuffle
        ys should not equal zs
      }
    }
  }

  "normalized" should "sum up to scale" in {
    forAll { map: Map[String, Double] =>
      whenever(map.values.sum != 0) {
        val scale = 10.0
        map.normalized(scale).values.sum should be (scale +- epsilon)
      }
    }
  }

  "merge" should "preserve keys from both maps and values from the second" in {
    forAll { (m1: Map[String, String], m2: Map[String, String]) =>
      val merged = m1 merge m2
      m1.keySet subsetOf  merged.keySet should be (true)
      m2.keySet subsetOf  merged.keySet should be (true)
      m2.values foreach { merged.values should contain (_) }
    }
  }

  "zipMap" should "contain only keys and values from both maps" in {
    forAll { (m1: Map[String, String], m2: Map[String, String]) =>
      val zipped = m1 zipMap m2
      zipped.keySet subsetOf m1.keySet should be (true)
      zipped.keySet subsetOf m2.keySet should be (true)
      zipped.values map { _._1 } foreach { m1.values should contain (_) }
      zipped.values map { _._2 } foreach { m2.values should contain (_) }
    }
  }

  "reversed" should "turn the keys of a map into values and reciprocally" in {
    forAll { map: Map[String, String] =>
      val reversed = map.reversed
      reversed.keys           should contain theSameElementsAs map.values.toSet
      reversed.values.flatten should contain theSameElementsAs map.keys
    }
  }

  "sorted" should "convert a Map to a SortedMap" in {
    forAll { map: Map[String, String] =>
      val sortedMap = map.sorted
      sortedMap should contain theSameElementsAs map
      sortedMap.keys.toSeq shouldBe sorted
    }
  }

  it should "convert a Set to a SortedSet" in {
    forAll { set: Set[String] =>
      val sortedSet = set.sorted
      sortedSet should contain theSameElementsAs set
      sortedSet.toSeq shouldBe sorted
    }
  }
}
