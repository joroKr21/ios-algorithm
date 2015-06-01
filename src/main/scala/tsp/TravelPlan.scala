package tsp

import org.coinor.opents._

case class TravelPlan(customers: Array[Array[Double]])
    extends SolutionAdapter {

  var tour = customers.indices.toArray

  override def clone = {
    val copy  = super.clone.asInstanceOf[TravelPlan]
    copy.tour = tour
    copy
  }

  override def toString = {
    val sb: StringBuffer = new StringBuffer
    sb append s"Solution value: ${getObjectiveValue()(0)}"
    sb append " Sequence: [ "

    for (x <- tour)
      if (x == 0) sb append x
      else sb append ", " append x

    sb append " ]"
    sb.toString
  }
}
