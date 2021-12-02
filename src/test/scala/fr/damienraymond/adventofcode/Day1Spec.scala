package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {

  behavior of "rateOfDepthIncrease"

  it should "compute the right depth for the example input" in {
    Day1.rateOfDepthIncrease(LazyList(199,200,208,210,200,207,240,269,260,263)) should be (7)
  }

  it should "return 0 if the input is empty" in {
    Day1.rateOfDepthIncrease(LazyList.empty) should be (0)
  }

  it should "return 1 with the list 199,200" in {
    Day1.rateOfDepthIncrease(LazyList(199,200)) should be (1)
  }

  it should "show the result" in {
    val res = Day1Input.input.use(numbers => IO(Day1.rateOfDepthIncrease(numbers))).unsafeRunSync()
    println(s"Day1.rateOfDepthIncrease(...) -> $res")
  }

  behavior of "rateOfDepthIncreaseSlidingWindow3"

  it should "return 0 if the input is <= 3" in {
    Day1.rateOfDepthIncreaseSlidingWindow3(LazyList.empty) should be (0)
    Day1.rateOfDepthIncreaseSlidingWindow3(LazyList(1)) should be (0)
    Day1.rateOfDepthIncreaseSlidingWindow3(LazyList(1,2)) should be (0)
    Day1.rateOfDepthIncreaseSlidingWindow3(LazyList(1,2,3)) should be (0)

  }

  it should "return 1 if the list is increasing List(199,200,208,210)" in {
    Day1.rateOfDepthIncreaseSlidingWindow3(LazyList(199,200,208,210)) should be (1)
  }

  it should "return 0 if the list is decreasing List(1,2,3,1)" in {
    Day1.rateOfDepthIncreaseSlidingWindow3(LazyList(1,2,3,1)) should be (0)
  }

  it should "compute the right depth for the example input (sliding window version)" in {
    Day1.rateOfDepthIncreaseSlidingWindow3(LazyList(199,200,208,210,200,207,240,269,260,263)) should be (5)
  }


  it should "show the result" in {
    val res = Day1Input.input.use(numbers => IO(Day1.rateOfDepthIncreaseSlidingWindow3(numbers))).unsafeRunSync()
    println(s"Day1.rateOfDepthIncreaseSlidingWindow3(...) -> $res")
  }

}
