package tsp

import org.coinor.opents.{Solution, Move}


case class MySwapMove(customer : Int, movement: Int) extends Move {
  override def operateOn(solution: Solution): Unit = solution match {
    case solution:MySolution => {
      val tour = solution.tour

      var pos1: Int = -1
      var pos2: Int = -1

      // Find positions
      var i: Int = 0
      while (i < tour.length && pos1 < 0) {
        if (tour(i) == customer) pos1 = i
        i+=1
      }
      pos2 = pos1 + movement

      // Swap
      val cust2: Int = tour(pos2)
      tour(pos1) = cust2
      tour(pos2) = customer
    }
  }

  /** Identify a move for SimpleTabuList */
  override def hashCode: Int = customer
}
