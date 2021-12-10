package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

object Day8 {

  def part1(input: String): Int =
    (parse _ andThen (_.map(_._2)) andThen countSimpleNumbers)(input)

  def part2(input: String): Int =
    (parse _ andThen countAllNumbers)(input)

  def parse(input: String): Vector[(Vector[String], Vector[String])] =
    input.split("\n").map(line => line.split(" *\\| *") match {
      case Array(firstPart, secondPart) =>
        val firstPartSplit = firstPart.split(" ").toVector
        val secondPartSplit = secondPart.split(" ").toVector
        (firstPartSplit, secondPartSplit)
    }).toVector

  def countSimpleNumbers(input: Vector[Vector[String]]): Int =
    input.map(_.map(_.length).count {
      case 2 | 4 | 3 | 7 => true
      case _ => false
    }).sum


  def countAllNumbers(input: Vector[(Vector[String], Vector[String])]): Int = {
    input.map{
      case (lineStart, lineStop) =>
        val line = Line(lineStart)
        lineStop.map(l => line.letters(l.toSet)).mkString("").toInt
    }
  }.sum

  case class Line(vector: Vector[String]) {
    lazy val sixLetterWords: Seq[String] = vector.filter(_.length == 6)
    lazy val fiveLetterWords: Seq[String] = vector.filter(_.length == 5)
    lazy val `0`: String = sixLetterWords
      .filterNot(_ == `9`)
      .find(letter => `7`.toSet.forall(letter.toSet.contains)).head
    lazy val `1`: String = vector.find(_.length == 2).get
    lazy val `2`: String = fiveLetterWords.find(_.toSet.contains(e.head)).head
    lazy val `3`: String = fiveLetterWords.filterNot(letter => letter == `2` || letter == `5`).head
    lazy val `4`: String = vector.find(_.length == 4).get
    lazy val `5`: String = fiveLetterWords.find(!_.toSet.contains(c.head)).head
    lazy val `6`: String = sixLetterWords.filterNot(letter => letter == `0` || letter == `9`).head
    lazy val `7`: String = vector.find(_.length == 3).get
    lazy val `8`: String = vector.find(_.length == 7).get
    lazy val `9`: String =
      sixLetterWords
        .find(letter => `4`.toSet.forall(letter.toSet.contains)).head

    lazy val letters: Map[Set[Char], Int] = Map(
      `0`.toSet -> 0,
      `1`.toSet -> 1,
      `2`.toSet -> 2,
      `3`.toSet -> 3,
      `4`.toSet -> 4,
      `5`.toSet -> 5,
      `6`.toSet -> 6,
      `7`.toSet -> 7,
      `8`.toSet -> 8,
      `9`.toSet -> 9,
    )

    lazy val a: String = (`7`.toSet diff `1`.toSet).head.toString
    lazy val b: String = (`4`.toSet diff `7`.toSet diff Set(d.head)).head.toString
    lazy val c: String = (`8`.toSet diff `6`.toSet).head.toString
    lazy val d: String = (`8`.toSet diff `0`.toSet).head.toString
    lazy val e: String = (`8`.toSet diff `9`.toSet).head.toString
    lazy val f: String = (`0`.toSet diff `2`.toSet diff Set(b.head)).head.toString
    lazy val g: String = (`9`.toSet diff `4`.toSet diff Set(a.head)).head.toString
  }

  val input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day8.txt")

}
