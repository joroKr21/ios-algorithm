package assigner

import org.coinor.opents.SolutionAdapter


class MatchingSolution(students: List[Student], groups: List[Group]) extends SolutionAdapter{
  override def clone = {
    val copy: MatchingSolution = super.clone.asInstanceOf[MatchingSolution]
    //copy.tour = tour

    copy
  }

//  override def toString: String = {
//    val s: StringBuffer = new StringBuffer
//    s.append("Solution value: " + getObjectiveValue()(0))
//    s.append(" Sequence: [ ")
//    tour.map { x =>
//      if (x == 0) {
//        s.append(x)
//      } else {
//        s.append(", ").append(x)
//      }
//    }
//    s.append(" ]")
//
//    s.toString
//  }
}

