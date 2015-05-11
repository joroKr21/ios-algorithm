package tsp

import org.coinor.opents.SolutionAdapter


case class MySolution(customers: Array[Array[Double]]) extends SolutionAdapter {
  var tour = (0 until customers.length).toArray

  override def clone = {
    val copy: MySolution = super.clone.asInstanceOf[MySolution]
    copy.tour = tour

    copy
  }

  override def toString: String = {
    val s: StringBuffer = new StringBuffer
    s.append("Solution value: " + getObjectiveValue()(0))
    s.append(" Sequence: [ ")
    tour.map { x =>
      if (x == 0) {
        s.append(x)
      } else {
        s.append(", ").append(x)
      }
    }
    s.append(" ]")

    s.toString
  }

}
