package tsp

import org.coinor.opents._

class Objective(customers: Array[Array[Double]])
    extends ObjectiveFunction {

  def evaluate(solution: Solution, move: Move) = solution match {
    case plan: TravelPlan =>
      val tour   = plan.tour
      val len    = tour.length
      val matrix = newMatrix

      move match {
        case swap: Swap if swap != null =>
          var pos1 = tour indexOf swap.customer
          var pos2 = pos1 + swap.movement
          // logic below requires pos1 < pos2
          if (pos1 > pos2) {
            val tmp = pos2
            pos2 = pos1
            pos1 = tmp
          }

          // prior objective value
          var dist = solution.getObjectiveValue()(0)
          // treat a pair swap move differently
          // A-B-C-D-E: swap C and D, say (works for symmetric matrix only)
          if (pos1 + 1 == pos2) {
            dist -= matrix(tour(pos1 - 1))(tour(pos1)) // -BC
            dist -= matrix(tour(pos2))(tour((pos2 + 1) % len)) // -DE
            dist += matrix(tour(pos1 - 1))(tour(pos2)) // +BD
            dist += matrix(tour(pos1))(tour((pos2 + 1) % len)) // +CE
            Array(dist)
          } else {
            // A-B-C-D-E-F: swap B and E, say
            dist -= matrix(tour(pos1 - 1))(tour(pos1)) // -AB
            dist -= matrix(tour(pos1))(tour(pos1 + 1)) // -BC
            dist -= matrix(tour(pos2 - 1))(tour(pos2)) // -DE
            dist -= matrix(tour(pos2))(tour((pos2 + 1) % len)) // -EF
            dist += matrix(tour(pos1 - 1))(tour(pos2)) // +AE
            dist += matrix(tour(pos2))(tour(pos1 + 1)) // +EC
            dist += matrix(tour(pos2 - 1))(tour(pos1)) // +DB
            dist += matrix(tour(pos1))(tour((pos2 + 1) % len)) // +BF
            Array(dist)
          }

        case _ =>
          val dist = 0 until len map { i =>
            matrix(tour(i))(if (i + 1 >= len) 0 else tour(i + 1))
          }

          Array(dist.sum)
      }
    }

  private def newMatrix = {
    val len    = customers.length
    val matrix = Array.ofDim[Double](len, len)

    for {
      i <- 0     until len
      j <- i + 1 until len
      Array(x1, y1) = customers(i)
      Array(x2, y2) = customers(j)
    } {
      matrix(j)(i) = norm(x1, y1, x2, y2)
      matrix(i)(j) = matrix(j)(i)
    }

    matrix
  }

  /** Calculate the distance between two points. */
  private def norm(x1: Double, y1: Double, x2: Double, y2: Double) = {
    val dx = x2 - x1
    val dy = y2 - y1
    math.sqrt(dx * dx + dy * dy)
  }
}
