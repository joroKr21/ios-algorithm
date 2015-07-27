package assigner

import assigner.model._

import org.scalacheck._
import org.scalacheck.Gen._

import scala.collection.JavaConversions._

trait DataGen {

  def minMaxGroupSizeGen(min: Int, max: Int):
      Gen[(Int, Int)] = for {
    minSize <- choose(min,     max)
    maxSize <- choose(minSize, max)
  } yield (minSize, maxSize)

  def normalizedWeightGen[A](set: Set[A], min: Double = 0, max: Double = 10):
      Gen[Map[A, Double]] = sequence {
    for (x <- set) yield for {
      weight <- choose(min, max)
    } yield (x, weight)
  } map { _.toMap }

  def normalizedLocalWeightGen(min: Double = 0, max: Double = 10):
      Gen[Map[String, Double]] =
    for (weights <- listOfN(2, choose(min, max)))
      yield Seq("groupPreferences",
                "friendsAndFoes").zip(weights).toMap normalized 1

  def normalizedGlobalWeightGen(min: Double = 0, max: Double = 10):
      Gen[Map[String, Double]] =
    for (weights <- listOfN(4, choose(min, max)))
      yield Seq("maximallyDiverse",
                "evenlySkilled",
                "groupPreferences",
                "friendsAndFoes").zip(weights).toMap normalized 1

  def permutationGen[A](seq: Seq[A]): Gen[Seq[A]] =
    oneOf(seq.permutations.toStream)

  def fixedSizeSubSeqGen[A](seq: Seq[A], n: Int): Gen[Seq[A]] =
    permutationGen(seq) map { _ take n }

  def minMaxSubSeqGen[A](seq: Seq[A], min: Int, max: Int):
      Gen[Seq[A]] = for {
    p <- permutationGen(seq)
    n <- choose(min, max)
  } yield p.take(n)

  def groupGen(
        n:            Int,
        skillGen:     Gen[Set[String]],
        sizeGen:      Gen[(Int, Int)],
        mandatoryGen: Gen[Boolean] = const(true)):
      Gen[List[Group]] = sequence {
    for (id <- 1 to n) yield for {
      (min, max) <- sizeGen
      name       <- alphaStr filter { !_.isEmpty } map capitalize
      mandatory  <- mandatoryGen
      skills     <- skillGen
    } yield Group(id, min, max, mandatory, name, skills)
  } map { _.toList }

  def studentGen(
        n:            Int,
        skills:       Set[String],
        friendGen:    Gen[Set[Long]],
        prefGen:      Gen[Map[Long, Double]],
        weightGen:    Gen[Map[String,  Double]] = normalizedLocalWeightGen(),
        mandatoryGen: Gen[Boolean]              = arbitrary[Boolean],
        skillGen:     Gen[Double]               = choose(1, 5),
        foeGen:       Gen[Set[Long]]       = const(Set.empty)):
      Gen[List[Student]] = sequence {
    for (id <- 1 to n) yield for {
      name        <- alphaStr filter { !_.isEmpty } map capitalize
      mandatory   <- mandatoryGen
      weights     <- weightGen
      preferences <- prefGen
      friends     <- friendGen
      foes        <- foeGen
      skillMap    <- for (ratings <- listOfN(skills.size, skillGen))
        yield (skills zip ratings).toMap
    } yield Student(id, name, mandatory, skillMap, weights, preferences, friends, foes)
  } map { _.toList }

  def courseGen(
        settings:       Settings,
        endpoints:      Endpoints,
        numStudentsGen: Gen[Int],
        numGroupsGen:   Gen[Int],
        numSkillsGen:   Gen[Int],
        groupSizeGen:   Gen[(Int, Int)],
        weightGen:      Gen[Map[String, Double]] = normalizedGlobalWeightGen()):
      Gen[Course] = for {
    id          <- posNum[Long]
    numStudents <- numStudentsGen
    numGroups   <- numGroupsGen
    numSkills   <- numSkillsGen
    skillGen     = alphaStr filter { !_.isEmpty } map { _.toLowerCase }
    skills      <- listOfN(numSkills, skillGen)
    subSkills   <- minMaxSubSeqGen(skills, 1, skills.size) map { _.toSet }
    weights     <- weightGen

    students   <- studentGen(
      n         = numStudents,
      skills    = skills.toSet,
      friendGen = listOf(choose(1l, numStudents.toLong)) map { _.toSet },
      prefGen   = normalizedWeightGen((1l to numGroups).toSet))

    groups    <- groupGen(
      n        = numGroups,
      skillGen = minMaxSubSeqGen(skills, 1, skills.size) map { _.toSet },
      sizeGen  = groupSizeGen)
  } yield Course(id, settings, endpoints, students, groups, subSkills, weights)

  def arbitrary[A: Arbitrary]: Gen[A] =
    implicitly[Arbitrary[A]].arbitrary

  private def capitalize(str: String) =
    str.head.toUpper +: str.tail.toLowerCase
}
