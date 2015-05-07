package tsp

import org.coinor.opents.SolutionAdapter


case class MySolution(customers: Array[Array[Double]]) extends SolutionAdapter {
  val tour = (0 until customers.length).toArray

  override def toString: String = {
    val s: StringBuffer = new StringBuffer
    s.append("Solution value: " + getObjectiveValue()(0))
    s.append(" Sequence: [ ")
    tour.map(x => s.append(x).append(", "))
    s.append(tour(tour.length - 1))
    s.append(" ]")

    s.toString
  }

}
