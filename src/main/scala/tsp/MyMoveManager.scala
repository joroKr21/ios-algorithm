package tsp

import org.coinor.opents.{Move, Solution, MoveManager}

class MyMoveManager extends MoveManager {
  override def getAllMoves(solution: Solution): Array[Move] = solution match {
    case solution:MySolution => {
      val tour = solution.tour

      val buffer = new Array[Move](tour.length * tour.length)
      var nextBufferPos: Int = 0

      for {
        i <- 1 until tour.length
        j <- -5 until 5
        if (i + j >= 1) && (i + j < tour.length) && (j != 0)
      } {
        nextBufferPos += 1
        buffer(nextBufferPos) = new MySwapMove(tour(i), j)
      }

      // Trim buffer
      val moves = new Array[Move](nextBufferPos)
      System.arraycopy(buffer, 0, moves, 0, nextBufferPos)

      moves
    }
  }
}
