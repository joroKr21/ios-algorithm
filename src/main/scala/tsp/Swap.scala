package tsp

import org.coinor.opents._

case class Swap(customer : Int, movement: Int) extends Move {
  def operateOn(solution: Solution) = solution match {
    case plan: TravelPlan =>
      val tour = plan.tour
      // find positions
      val pos1 = tour indexOf customer
      val pos2 = pos1 + movement
      // swap
      tour(pos1) = tour(pos2)
      tour(pos2) = customer
  }

  /** Identify a move for [[SimpleTabuList]]. */
  override def hashCode = customer
}
