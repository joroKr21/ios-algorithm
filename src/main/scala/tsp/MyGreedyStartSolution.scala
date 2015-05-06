package tsp

class MyGreedyStartSolution(customers: Array[Array[Double]]) extends MySolution(customers) {
  override val tour : Array[Int] = {
    val avail = (0 until customers.length).toArray
    val tour = Array[Int](customers.length)

    var closest = -1
    var dist = Double.MaxValue
    for {
      i <- 1 until customers.length
    }
    {
      closest = -1
      dist = Double.MaxValue
      (1 until customers.length).filter(j => (norm(customers, tour(i - 1), j) < dist) && (avail(j) >= 0)).map { x =>
        dist = norm(customers, tour(i - 1), x)
        closest = x
      }

      avail(closest) = -1
      tour(i) = closest
    }

    tour
  }

  private def norm(matr: Array[Array[Double]], a: Int, b: Int): Double = {
    val xDiff: Double = matr(b)(0) - matr(a)(0)
    val yDiff: Double = matr(b)(1) - matr(a)(1)

    Math.sqrt(xDiff * xDiff + yDiff * yDiff)
  }
}
