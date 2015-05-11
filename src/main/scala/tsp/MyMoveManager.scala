package tsp

import org.coinor.opents.{Move, Solution, MoveManager}

class MyMoveManager extends MoveManager {
  override def getAllMoves(solution: Solution): Array[Move] = solution match {
    case solution:MySolution => {
      val tour = solution.tour

      val buffer = Array.ofDim[Move](tour.length * tour.length)
      var nextBufferPos: Int = 0

      for {
        i <- 1 until tour.length
        j <- -5 until 6
        if (i + j >= 1) && (i + j < tour.length) && (j != 0)
      } {
        buffer(nextBufferPos) = new MySwapMove(tour(i), j)
        nextBufferPos += 1
      }

      // Trim buffer
      val moves = Array.ofDim[Move](nextBufferPos)
      System.arraycopy(buffer, 0, moves, 0, nextBufferPos)

      moves
    }
  }
}
