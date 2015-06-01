package tsp

import org.coinor.opents.{Move, Solution, MoveManager}

class Manager extends MoveManager {
  def getAllMoves(solution: Solution) = solution match {
    case plan: TravelPlan => {
      val tour    = plan.tour
      val buffer  = Array.ofDim[Move](tour.length * tour.length)
      var nextPos = 0

      for {
        i <-  1 until tour.length
        j <- -5 until 6
        if (i + j >= 1) && (i + j < tour.length) && (j != 0)
      } {
        buffer(nextPos) = Swap(tour(i), j)
        nextPos += 1
      }

      // trim buffer
      val moves = Array.ofDim[Move](nextPos)
      System.arraycopy(buffer, 0, moves, 0, nextPos)
      moves
    }
  }
}
