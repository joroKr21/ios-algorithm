package tsp

import org.coinor.opents.{Move, ObjectiveFunction, Solution}

class MyObjectiveFunction(customers: Array[Array[Double]]) extends ObjectiveFunction {
  override def evaluate(solution: Solution, move: Move): Array[Double] = {
    val tour: Array[Int] = solution.asInstanceOf[MySolution].tour
    val len: Int = tour.length
    val matrix = createMatrix()


    move match {
      case move:MySwapMove =>
        var pos1: Int = -1
        var pos2: Int = -1

        pos1 = tour.indexOf(move.customer)
        pos2 = pos1 + move.movement

        // Logic below requires pos1 < pos2
        if (pos1 > pos2) {
          val temp = pos2
          pos2 = pos1
          pos1 = temp
        }

        // Prior objective value
        var dist: Double = solution.getObjectiveValue()(0)

        // Treat a pair swap move differently
        //     | |
        // A-B-C-D-E: swap C and D, say (works for symmetric matrix only)
        if (pos1 + 1 == pos2) {
          dist -= matrix(tour(pos1 - 1))(tour(pos1))            // -BC
          dist -= matrix(tour(pos2))(tour((pos2 + 1) % len))    // -DE
          dist += matrix(tour(pos1 - 1))(tour(pos2))            // +BD
          dist += matrix(tour(pos1))(tour((pos2 + 1) % len))    // +CE

          Array(dist)
        }

        //   |     |
        // A-B-C-D-E-F: swap B and E, say
        else {
          dist -= matrix(tour(pos1 - 1))(tour(pos1))            // -AB
          dist -= matrix(tour(pos1))(tour(pos1 + 1))            // -BC
          dist -= matrix(tour(pos2 - 1))(tour(pos2))            // -DE
          dist -= matrix(tour(pos2))(tour((pos2 + 1) % len))    // -EF

          dist += matrix(tour(pos1 - 1))(tour(pos2))            // +AE
          dist += matrix(tour(pos2))(tour(pos1 + 1))            // +EC
          dist += matrix(tour(pos2 - 1))(tour(pos1))            // +DB
          dist += matrix(tour(pos1))(tour((pos2 + 1) % len))    // +BF

          Array(dist)
        }


      case _ =>
        val dist = (0 until len).map(i => matrix(tour(i))(if (i + 1 >= len) 0 else tour(i + 1))).sum

        Array(dist)
    }
  }

  def createMatrix() : Array[Array[Double]] = {
    val len: Int = customers.length
    val matrix = Array.ofDim[Double](len, len)
    for {
      i <- 0 until len
      j <- (i+1) until len
    } {
      matrix(i)(j) = {
        matrix(j)(i) = norm(customers(i)(0), customers(i)(1), customers(j)(0), customers(j)(1));
        matrix(j)(i)
      }
    }

    matrix
  }

  /** Calculate distance between two points. */
  private def norm(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    val xDiff: Double = x2 - x1
    val yDiff: Double = y2 - y1

    Math.sqrt(xDiff * xDiff + yDiff * yDiff)
  }
}
