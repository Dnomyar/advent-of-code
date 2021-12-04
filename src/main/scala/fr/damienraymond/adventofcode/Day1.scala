package fr.damienraymond.adventofcode

import fr.damienraymond.ReadFileUtil

object Day1 {

  def rateOfDepthIncrease(input: LazyList[Int]): Int =
      rateOfDepthIncreaseLazy(input)

  def rateOfDepthIncreaseSlidingWindow3(input: LazyList[Int]): Int =
    rateOfDepthIncreaseLazy(sumSlidingWindowOf(3)(input))

  private def sumSlidingWindowOf(size: Int)(input: LazyList[Int]): LazyList[Int] =
    input.sliding(size).map(_.sum).to(LazyList)

  private def rateOfDepthIncreaseLazy(streamInput: LazyList[Int]) =
    //streamInput.zip(streamInput.drop(1)).map {
    streamInput.sliding(2).map {
      case LazyList(previous, current) if previous < current => 1
      case _ => 0
    }.sum
}

object Day1Input {

  val input = ReadFileUtil.readFileLineInts("day1.txt")

}
