import scala.util.Random

/** Syntactic sugar and utility methods. */
package object assigner {

  /**
   * Flatten all [[Traversable]] arguments into a single sequence.
   * @param xs sequences to flatten into one
   * @tparam A greatest common denominator of the element type
   * @return a single sequence containing all original elements in order
   */
  def flatten[A](xs: Traversable[A]*): Seq[A] = xs.flatten

  implicit class Singleton[A](val self: A) extends AnyVal {
    /** @return a singleton [[List]] containing this element */
    def sng: List[A] = self :: Nil
  }

  implicit class Frequencies[A](val self: Seq[A]) extends AnyVal {
    
    /**
     * Count the number of occurrences of elements in this sequence.
     * @return a [[Map]] with counts of the occurrences of each element
     */
    def freq: Map[A, Int] = freqBy(identity)

    /**
     * Count the number of occurrences of each key in this sequence.
     * @param k function to extract the key from each element
     * @tparam K the type of key to extract
     * @return a [[Map]] with counts of the occurrences of each key
     */
    def freqBy[K](k: A => K): Map[K, Int] =
      self groupBy k mapValues { _.size }
  }

  implicit class MeanVar[A](val self: Traversable[A]) extends AnyVal {
    import Fractional.Implicits._

    /** @return the mean of all elements in this sequence */
    def mean(implicit f: Fractional[A]): A =
      self.sum / f.fromInt(self.size)

    /** @return the variance of all elements in this sequence */
    def variance(implicit f: Fractional[A]): A = {
      def sqr(x: A) = x * x
      val m         = self.mean
      self.map { x => sqr(x - m) }.mean
    }
  }

  implicit class SumProduct[A](val self: Seq[A]) extends AnyVal {

    /**
     * Sum all elements of this sequence by using a conversion function.
     * @param f function to convert each element into a [[Numeric]]
     * @tparam B type of [[Numeric]] to sum
     * @return the sum of all converted elements
     */
    def sumBy[B: Numeric](f: A => B): B = self.map(f).sum

    /**
     * Multiply all elements of this sequence by using a conversion function.
     * @param f function to convert each element into a [[Numeric]]
     * @tparam B type of [[Numeric]] to multiply
     * @return the product of all converted elements
     */
    def productBy[B: Numeric](f: A => B): B = self.map(f).product
  }

  implicit class Shuffle[A](val self: List[A]) extends AnyVal {
    /** @return this [[List]] in arbitrary order */
    def shuffle: List[A] = Random shuffle self
  }

  implicit class Normalize[K, V](val self: Map[K, V]) extends AnyVal {
    import Fractional.Implicits._

    /**
     * Normalize all values in a [[Map]] according to a scale.
     * @param scale all values will lie in [-scale, scale]
     * @param f     implicit [[Fractional]] type class
     * @return this [[Map]] with its values normalized
     */
    def normalized(scale: V)(implicit f: Fractional[V]): Map[K, V] = {
      val sum = self.values.sum / scale
      if (sum == f.fromInt(0)) self
      else self mapValues { _ / sum }
    }
  }

  implicit class Merge[K, V](val self: Map[K, V]) extends AnyVal {

    /**
     * Merge with another [[Map]]. Collisions are resolved by overwriting.
     * @param that [[Map]] to merge with
     * @return a combination of both [[Map]]s that contains all keys
     */
    def merge(that: Map[K, V]) =
      (self.keySet | that.keySet).map { key =>
        key -> that.getOrElse(key, self(key))
      }.toMap
  }

  implicit class ZipMap[K, V](val self: Map[K, V]) extends AnyVal {

    /**
     * Zip with another [[Map]] by keeping only common keys.
     * @param that [[Map]] to zip with
     * @return a combination of both [[Map]]s that contains common keys
     */
    def zipMap[V2](that: Map[K, V2]) =
      (self.keySet & that.keySet).map { key =>
        key -> (self(key), that(key))
      }.toMap
  }
  
  implicit class Reverse[K, V](val self: Map[K, V]) extends AnyVal {
    /** @return a new [[Map]] with the keys and values swapped */
    def reversed: Map[V, Set[K]] =
      self groupBy { _._2 } mapValues { _.keySet }
  }
}