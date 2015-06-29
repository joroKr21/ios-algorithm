package assigner.search.tabu

import org.coinor.opents._

import scala.collection.mutable

class TabuQueue(size: Int) extends TabuList {
  val queue = mutable.Queue.empty[Move]

  def setTabu(fromSolution: Solution, move: Move) = {
    queue.enqueue(move)
    if (queue.size > size) queue.dequeue()
  }

  def isTabu(fromSolution: Solution, move: Move) =
    queue.contains(move)
}
