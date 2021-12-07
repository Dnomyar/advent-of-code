package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

object Day7 {

  def part1(string: String): Int = (parse _ andThen findTheLowestCostOfFuelPart1) (string)

  def parse(string: String): Vector[Int] =
    string.split(",").map(_.toInt).toVector

  def findTheLowestCostOfFuelPart1(crabPosition: Vector[Int]): Int = {
    val size = crabPosition.size
    //    crabPosition.sum / size

    val sorted = crabPosition.sorted
    val median = sorted(size / 2)

    crabPosition.map(c => Math.abs(c - median)).sum
  }

  val day7Input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day7.txt")


  def findTheLowestCostOfFuelPart2(crabPosition: Vector[Int]): Int = {
    val size = crabPosition.size
    //    crabPosition.sum / size

    val sorted = crabPosition.sorted
    val median = sorted(size / 2)

    val sum: Int => Int = n => (n * (n + 1)) / 2

    def extimateRequiedFuel(median: Int) = {
      crabPosition.map(c => sum(Math.abs(c - median))).sum
    }

    (median - 1000 to median + 1000).map(extimateRequiedFuel).min

  }

  def part2(string: String): Int = (parse _ andThen findTheLowestCostOfFuelPart2) (string)

}
