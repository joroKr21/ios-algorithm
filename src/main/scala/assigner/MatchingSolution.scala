package assigner

import org.coinor.opents.SolutionAdapter


class MatchingSolution(students: List[Student], groups: List[Group]) extends SolutionAdapter{
  var matching : Map[Int, Int] = Map()

  override def clone = {
    val copy: MatchingSolution = super.clone.asInstanceOf[MatchingSolution]
    copy.matching = matching

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

