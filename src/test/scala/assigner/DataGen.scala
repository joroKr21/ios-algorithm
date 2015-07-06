package assigner

import org.scalacheck._
import org.scalacheck.Gen._

trait DataGen {

  def minMaxGroupSizeGen(min: Int, max: Int) = for {
    minSize <- choose(min, max)
    maxSize <- choose(minSize, max)
  } yield (minSize, maxSize)

  def normalizedWeightGen(min: Int, max: Int) = for {
    preferences <- choose(min, max)
    friends <- choose(min, max)
    sum = (preferences + friends).toDouble
    ps = preferences / sum
    fs = friends / sum
  } yield Map("preferences" -> ps, "friends" -> fs)

  def permutationGen[A](seq: Seq[A]) =
    oneOf(seq.permutations.toStream)

  def fixedSizeSubSeqGen[A](seq: Seq[A], n: Int) =
    permutationGen(seq) map { _ take n }

  def minMaxSubSeqGen[A](seq: Seq[A], min: Int, max: Int) = for {
    p <- permutationGen(seq)
    n <- choose(min, max)
  } yield p take n

  def groupGen(n: Int,
               sizeGen: Gen[(Int, Int)],
               skillGen: Gen[Set[String]]) = for {
    id <- 1 to n
    (min, max) <- sizeGen
    name <- alphaStr map capitalize
    skills <- skillGen
  } yield Group(id, min, max, name, skills)

  def studentGen(n: Int,
                 skills: Set[String],
                 mandatoryGen: Gen[Boolean],
                 skillGen: Gen[Int],
                 weightGen: Gen[Map[String, Double]],
                 prefGen: Gen[List[Int]],
                 friendGen: Gen[Set[Int]],
                 foeGen: Gen[Set[Int]]) = for {
    id <- 1 to n
    name <- alphaStr map capitalize
    mandatory <- mandatoryGen
    weights <- weightGen
    preferences <- prefGen
    friends <- friendGen
    foes <- foeGen
    skillMap <- for (ratings <- listOfN(skills.size, skillGen))
      yield (skills zip ratings).toMap
  } yield Student(
      id, name, mandatory, skillMap, weights, preferences, friends, foes)

  def courseGen(settings: Settings,
                numGroupGen: Gen[Int],
                numStudentGen: Gen[Int],
                numSkillsGen: Gen[Int],
                studentGen: Gen[List[Student]],
                groupGen: Gen[List[Group]]) = for {
    id <- posNum[Int]
    numGroups <- numGroupGen
    numStudents <- numStudentGen
    numSkills <- numSkillsGen
    skills <- listOfN(numSkills, alphaStr map { _.toLowerCase })
    students <- studentGen
    groups <- groupGen
  } yield Course(id, settings, students, groups, skills.toSet)

  private def capitalize(str: String) =
    str.head.toUpper +: str.tail.toLowerCase
}
