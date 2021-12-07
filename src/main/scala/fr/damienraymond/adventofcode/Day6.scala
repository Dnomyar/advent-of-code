package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

object Day6 {

  type NumberOfFish = Long
  type Day = Long

  def part1(input: String)(numberOfDays: Long): NumberOfFish =
    (parse _ andThen countFishPerDay andThen numberOrFishAfter2(numberOfDays) andThen numberOfFishAtTheEnd)(input)

  def parse(input: String): List[Long] = input.split(",").map(_.toLong).toList

  def countFishPerDay(fishCycle: List[Long]): Vector[NumberOfFish] = {
    val numberOfFishPerDay = fishCycle.groupBy(identity).view.mapValues(_.size.toLong).toMap
    Vector.range[Long](0L, 10L).map(numberOfFishPerDay.getOrElse(_, 0L))
  }

  def numberOrFishAfter2(numberOfDays: Long)(initialState: Vector[NumberOfFish]): Vector[NumberOfFish] = {
    require(initialState.length == 10)

    val addNewBirths: Vector[Long] => Vector[Long] = fishVector =>
      fishVector.updated(9, fishVector(0))

    val move0sTo7s: Vector[Long] => Vector[Long] = fishVector =>
      fishVector.updated(7, fishVector(0) + fishVector(7))

    val shiftAllByOne: Vector[Long] => Vector[Long] =
      (0 to 9).foldLeft(_){
        case (acc, day) if day == 9 =>
          acc.updated(9,0)
        case (acc, day) =>
          acc.updated(day, acc(day + 1))
      }

    val applyOneDay = addNewBirths andThen move0sTo7s andThen shiftAllByOne

    val newState = applyOneDay(initialState)

    if(numberOfDays == 1) newState
    else numberOrFishAfter2(numberOfDays - 1)(newState)
  }

  def numberOfFishAtTheEnd(state: Vector[NumberOfFish]): Long = {
    state.sum
  }


  val day6Input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day6.txt")


}
