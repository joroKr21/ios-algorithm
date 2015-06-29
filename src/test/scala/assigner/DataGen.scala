package assigner

import assigner.model._

import org.scalacheck._
import org.scalacheck.Gen._

trait DataGen {

  def minMaxGroupSizeGen(min: Int, max: Int) = for {
    minSize <- choose(min, max)
    maxSize <- choose(minSize, max)
  } yield (minSize, maxSize)

  def normalizedWeightGen(min: Double, max: Double) = for {
    preferences <- choose(min, max)
    friends <- choose(min, max)
    sum = preferences + friends
    ps = preferences / sum
    fs = friends / sum
  } yield Map("preferences" -> ps, "friends" -> fs)

  def permutationGen[A](seq: Seq[A]) =
    oneOf(seq.permutations.toStream)

  def fixedSizeSubSeqGen[A](seq: Seq[A], n: Int) =
    permutationGen(seq).map(_.take(n))

  def minMaxSubSeqGen[A](seq: Seq[A], min: Int, max: Int) = for {
    p <- permutationGen(seq)
    n <- choose(min, max)
  } yield p.take(n)

  def groupGen(n: Int,
               sizeGen: Gen[(Int, Int)],
               mandatoryGen: Gen[Boolean],
               skillGen: Gen[Set[String]]) = for {
    id <- 1 to n
    (min, max) <- sizeGen
    name <- alphaStr map capitalize
    mandatory <- mandatoryGen
    skills <- skillGen
  } yield Group(id, min, max, mandatory, name, skills)

  def studentGen(n: Int,
                 skills: Set[String],
                 mandatoryGen: Gen[Boolean],
                 skillGen: Gen[Double],
                 weightGen: Gen[Map[String, Double]],
                 prefGen: Gen[Map[GroupId, Double]],
                 friendGen: Gen[Set[StudentId]],
                 foeGen: Gen[Set[StudentId]]) = for {
    id <- 1 to n
    name <- alphaStr map capitalize
    mandatory <- mandatoryGen
    weights <- weightGen
    preferences <- prefGen
    friends <- friendGen
    foes <- foeGen
    skillMap <- for (ratings <- listOfN(skills.size, skillGen))
      yield skills.zip(ratings).toMap
  } yield Student(id, name, mandatory, skillMap, weights, preferences, friends, foes)

  def courseGen(settings: Settings,
                endpoints: Endpoints,
                numGroupsGen: Gen[Int],
                numStudentsGen: Gen[Int],
                numSkillsGen: Gen[Int],
                studentGen: Gen[List[Student]],
                groupGen: Gen[List[Group]]) = for {
    id <- posNum[Long]
    numGroups <- numGroupsGen
    numStudents <- numStudentsGen
    numSkills <- numSkillsGen
    skills <- listOfN(numSkills, alphaStr.map(_.toLowerCase))
    students <- studentGen
    groups <- groupGen
  } yield Course(id, settings, endpoints, students, groups, skills.toSet)

  private def capitalize(str: String) =
    str.head.toUpper +: str.tail.toLowerCase
}
